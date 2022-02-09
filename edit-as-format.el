;;; edit-as-format.el --- Edit document as other format  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <jingxiaobing@gmail.com>
;; Keywords: org markdown

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

(defcustom edit-as-format-filters-folder
  (expand-file-name
   "filters" (file-name-directory (or buffer-file-name load-file-name)))
  "Location of Lua filters for use with pandoc."
  :type 'directory
  :group 'edit-as-format)

(defcustom edit-as-format-pandoc-executable "pandoc"
  "Pandoc executable."
  :type 'string
  :group 'edit-as-format)

(defcustom edit-as-format-lua-filters
  (directory-files edit-as-format-filters-folder t "^_.*\\.lua")
  "List of filters to apply to all backends.

By default, all lua files starting with '_' in `edit-as-format-filters-folder'
are used."
  :type '(set string)
  :group 'edit-as-format)

(defun edit-as-format--pandoc-formats ()
  "Get pandoc supported formats."
  (unwind-protect
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
          (cl-intersection input-formats output-formats :test 'equal)))
    '("org" "rst" "gfm")))

(defcustom edit-as-format-formats
  (edit-as-format--pandoc-formats)
  "Supported formats."
  :type '(set string)
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

(defun edit-as-format (tgt)
  "Edit as format, choose target format TGT from prompt."
  (interactive
   (list (completing-read "Format: " edit-as-format-formats nil t)))
  (edit-as-format-edit (edit-as-format--guess-buffer-format) tgt))

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
  "Convert buffer region(BEG END) from SRC format to TGT format."
  (let* ((content (buffer-substring-no-properties beg end))
         (converted (edit-as-format--convert-string content src tgt))
         (beg-marker (copy-marker beg))
         (end-marker (copy-marker end)))
    (save-match-data ;; replace old `content' with `converted'
      (set-match-data (list beg-marker end-marker))
      (unless (string= converted (match-string 0))
        (replace-match converted t t)))))

(defun edit-as-format--restore-commited-buffer (beg end)
  "Restore buffer format, called from parent-buffer."
  (when-let* ((overlay (edit-indirect--search-for-edit-indirect beg end))
              (indirect-buffer (overlay-get overlay 'edit-indirect-buffer))
              (src (buffer-local-value 'edit-as-format--src indirect-buffer))
              (tgt (buffer-local-value 'edit-as-format--tgt indirect-buffer)))
    (edit-as-format--convert-buffer beg end tgt src)))

(defun edit-as-format-edit (src tgt)
  "Edit buffer which is SRC format, as other TGT format."
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max)))
        (restore-format (lambda (beg1 end1) (edit-as-format--convert-buffer beg1 end1 tgt src))))
    (unless (and (member src edit-as-format-formats)
                 (member tgt edit-as-format-formats))
      (error "Format not recognized, src: %s, tgt: %s" src tgt))
    (add-hook 'edit-indirect-after-commit-functions
              'edit-as-format--restore-commited-buffer nil 'local)
    (let ((indirect-buffer (edit-indirect-region beg end t)))
      (with-current-buffer indirect-buffer
        (edit-as-format--convert-buffer 1 (point-max) src tgt)
        (if-let ((mode-func (alist-get tgt edit-as-format-major-modes nil nil 'equal)))
            (funcall mode-func)
          (message "Cannot find major mode for `%s', please check `edit-as-format-major-modes'" tgt))
        (set (make-local-variable 'edit-as-format--src) src)
        (set (make-local-variable 'edit-as-format--tgt) tgt)))))

(defun edit-as-org ()
  "Edit as org mode."
  (interactive)
  (edit-as-format-edit (edit-as-format--guess-buffer-format) "org"))

(provide 'edit-as-format)
;;; edit-as-format.el ends here
