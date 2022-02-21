;;; edit-as-format.el --- Edit document as other format  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author: Xiaobing Jing <jingxiaobing@gmail.com>
;; Keywords: files, outlines, convenience
;; Created: Feb 2022
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (edit-indirect "0.1.5"))
;; URL: https://github.com/etern/edit-as-format

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Edit document as other format, using pandoc

;;; Code:
(require 'edit-indirect)
(eval-when-compile (require 'subr-x))

(defgroup edit-as-format nil
  "Edit document as other format"
  :group 'editing)

(defconst edit-as-format-filters-folder
  (directory-file-name
   (expand-file-name
    "filters" (file-name-directory (or load-file-name buffer-file-name))))
  "Location of Lua filters for use with pandoc.")

(defcustom edit-as-format-pandoc-executable "pandoc"
  "Pandoc executable."
  :type 'string
  :group 'edit-as-format)

(defcustom edit-as-format-lua-filters
  (directory-files edit-as-format-filters-folder t "^_.*\\.lua")
  "List of filters to apply to all backends.

By default, all lua files starting with '_' in `edit-as-format-filters-folder'
are used."
  :type '(repeat string)
  :group 'edit-as-format)

(defvar edit-as-format--src nil
  "Source format, buffer local in indirect-buffer.")

(defvar edit-as-format--tgt nil
  "Target format, buffer local in indirect-buffer.")

(defun edit-as-format--pandoc-formats ()
  "Get pandoc supported formats."
  (or (ignore-errors
        (with-temp-buffer
          (let ((input-formats
                 (progn
                   (call-process edit-as-format-pandoc-executable nil '(t nil) nil "--list-input-formats")
                   (split-string (buffer-substring-no-properties 1 (point-max)))))
                (output-formats
                 (progn
                   (erase-buffer)
                   (call-process edit-as-format-pandoc-executable nil '(t nil) nil "--list-output-formats")
                   (split-string (buffer-substring-no-properties 1 (point-max))))))
            (cl-intersection input-formats output-formats :test 'equal))))
      '("org" "rst" "gfm" "markdown")))

(defcustom edit-as-format-formats
  (edit-as-format--pandoc-formats)
  "Supported formats."
  :type '(repeat string)
  :group 'edit-as-format)

(defcustom edit-as-format-major-modes
  '(("org" . org-mode)
    ("gfm" . markdown-mode)
    ("markdown" . markdown-mode)
    ("markdown_github" . markdown-mode))
  "Format to `major-mode' mapping."
  :type '(alist :value-type symbol)
  :group 'edit-as-format)

(defun edit-as-format--guess-buffer-format ()
  "Guess current buffer format."
  (let ((ext (file-name-extension (buffer-name))))
    (pcase ext
      ((or "md" "markdown") "gfm")
      ("org" "org")
      ("rst" "rst")
      ((or "tex" "latex" "latex"))
      (_ (progn (message "failed to guess buffer file format.") nil)))))

(defun edit-as-format--convert-string (content src tgt)
  "Convert string CONTENT from SRC format to TGT format."
  (let* ((filters (mapcan (lambda (filter) (list "--lua-filter" filter))
                          edit-as-format-lua-filters))
         (args (append filters (list "-f" src "-t" tgt))))
    (with-temp-buffer
      (apply #'call-process-region content nil
             edit-as-format-pandoc-executable nil '(t nil) nil args)
      (buffer-substring-no-properties (point-min) (point-max)))))

;; didn't find a way to convert buffer directly without affecting
;; overlay, so convert string first
(defun edit-as-format--convert-buffer (beg end src tgt)
  "Convert buffer region BEG..END from SRC format to TGT format."
  (let* ((content (buffer-substring-no-properties beg end))
         (converted (edit-as-format--convert-string content src tgt))
         (beg-marker (copy-marker beg))
         (end-marker (copy-marker end)))
    (save-match-data ;; replace old `content' with `converted'
      (set-match-data (list beg-marker end-marker))
      (unless (string= converted (match-string 0))
        (replace-match converted t t)))))

(defun edit-as-format--restore-commited-buffer (beg end)
  "Restore buffer format, called from parent-buffer.
BEG..END is the region to be handled."
  (when-let* ((overlay (edit-indirect--search-for-edit-indirect beg end))
              (indirect-buffer (overlay-get overlay 'edit-indirect-buffer))
              (src (buffer-local-value 'edit-as-format--src indirect-buffer))
              (tgt (buffer-local-value 'edit-as-format--tgt indirect-buffer)))
    (edit-as-format--convert-buffer beg end tgt src)))

(defun edit-as-format--setup-indirect-buffer (src tgt buffer)
  "Convert BUFFER from SRC to TGT, and let it remember these formats."
  (with-current-buffer buffer
    (edit-as-format--convert-buffer 1 (point-max) src tgt)
    (if-let ((mode-func (alist-get tgt edit-as-format-major-modes nil nil 'equal)))
        (funcall mode-func)
      (message "Cannot find major mode for `%s', please check `edit-as-format-major-modes'" tgt))
    (set (make-local-variable 'edit-as-format--src) src)
    (set (make-local-variable 'edit-as-format--tgt) tgt)))

(defun edit-as-format--edit (src tgt)
  "Edit buffer which is SRC format, as other TGT format."
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (unless (and (member src edit-as-format-formats)
                 (member tgt edit-as-format-formats))
      (error "Format not recognized, src: %s, tgt: %s" src tgt))
    (when (= beg end) ;; for empty buffer
      (goto-char end)
      (insert "\n")
      (setq end (1+ end)))
    (add-hook 'edit-indirect-after-commit-functions
              #'edit-as-format--restore-commited-buffer nil 'local)
    (let ((buffer (edit-indirect-region beg end t)))
      (condition-case err
          (edit-as-format--setup-indirect-buffer src tgt buffer)
        (error (with-current-buffer buffer
                 (edit-indirect--abort)
                 (message (error-message-string err))))))))

;;;###autoload
(defun edit-as-format (&optional src tgt)
  "Edit current buffer as other format.

if SRC format is not provided, make a guess
if TGT format is not provided, prompt for confirm."
  (interactive)
  (let ((src (if (null src) (edit-as-format--guess-buffer-format) src))
        (tgt (if (null tgt)
                 (completing-read "Format: " edit-as-format-formats nil t)
               tgt)))
    (edit-as-format--edit src tgt)))

;;;###autoload
(defun edit-as-format-org ()
  "Edit as org mode."
  (interactive)
  (edit-as-format nil "org"))

(provide 'edit-as-format)
;;; edit-as-format.el ends here
