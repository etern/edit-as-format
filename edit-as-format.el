;;; edit-as-format.el --- Edit file as other format  -*- lexical-binding: t; -*-

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

;; Edit file as other format, using pandoc

;;; Code:
(require 'edit-indirect)

(defcustom edit-as-format-filters-folder
  (expand-file-name
   "filters" (file-name-directory (or buffer-file-name load-file-name)))
  "Location of Lua filters for use with pandoc.
If FITERS/backend.lua exists, it will automatically be used when backend is registered."
  :type 'string
  :group 'edit-as-format)

(defcustom edit-as-format-pandoc-executable "pandoc"
  "Pandoc executable"
  :type 'string
  :group 'edit-as-format)

(defcustom edit-as-format-lua-filters
  (directory-files edit-as-format-filters-folder t "^_.*\\.lua")
  "List of filters to apply to all backends.

By default, all lua files starting with '_' in `edit-as-format-filters-folder'
are used."
  :type 'list
  :group 'edit-as-format)

(defvar edit-as-format-formats
  '(("org" . org)
    ("markdown-github" . gfm)
    ("rst" . rst)))

(defvar edit-as-format-major-modes
  '((org . org-mode)
    (gfm . markdown-mode)))

(defun edit-as-format--get-buffer-format ()
    "Get current buffer format"
  (let ((ext (file-name-extension (buffer-name))))
    (pcase ext
      ((or "md" "markdown") 'gfm)
      ("org" 'org)
      ("rst" 'rst))))

(defun edit-as-format (format)
  "Edit as format, choose format from prompt"
  (interactive
   (list (alist-get (completing-read "Format: " edit-as-format-formats nil t)
                    edit-as-format-formats nil nil #'equal)))
  (edit-as-format--edit (edit-as-format--get-buffer-format) format))

(defun edit-as-format--convert-string (content src tgt)
  "Convert string"
  (let* ((filters (mapcan (lambda (filter) (list "--lua-filter" filter))
                          edit-as-format-lua-filters))
         (args (append filters (list "-f" (symbol-name src) "-t" (symbol-name tgt)))))
    (with-temp-buffer
      (apply #'call-process-region content nil
             edit-as-format-pandoc-executable nil '(t nil) nil args)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun edit-as-format--convert-buffer (beg end src tgt)
  "Convert buffer"
  (let* ((content (buffer-substring-no-properties beg end))
         (converted (edit-as-format--convert-string content src tgt))
         (beg-marker (copy-marker beg))
         (end-marker (copy-marker end)))
    (save-match-data ;; replace old `content' with `converted'
      (set-match-data (list beg-marker end-marker))
      (unless (string= converted (match-string 0))
        (replace-match converted t t)))))

(defun edit-as-format--edit (src tgt)
  "Edit buffer as other FORMAT"
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max)))
        (convert-format (lambda () (edit-as-format--convert-buffer (point-min) (point-max) src tgt)
                          (when-let ((major-mode-func (alist-get tgt edit-as-format-major-modes)))
                            (funcall major-mode-func))))
        (restore-format (lambda (beg1 end1) (edit-as-format--convert-buffer beg1 end1 tgt src))))
    (add-hook 'edit-indirect-after-creation-hook convert-format nil 'local)
    (add-hook 'edit-indirect-after-commit-functions restore-format nil 'local)
    (edit-indirect-region beg end t)))

(defun edit-as-org ()
  "Edit as org mode"
  (interactive)
  (edit-as-format--edit (edit-as-format--get-buffer-format) 'org))

(provide 'edit-as-format)
;;; edit-as-format.el ends here
