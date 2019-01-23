;;; ds-util.el --- utility functions                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Paul B Davis

;; Author:  <paul@dangersalad.com>

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

;; 

;;; Code:

(defun ds/indent-buffer ()
  "Indent entire buffer using `indent-according-to-mode'."
  (interactive)
  (save-excursion
    (push-mark (point))
    (push-mark (point-max) nil t)
    (goto-char (point-min))
    (indent-region (region-beginning) (region-end))))

(defun ds/set-local-variable (varname value)
  "Make a variable VARNAME local to the buffer if needed, then set to VALUE."
  (interactive "vVariable Name: \nsNew Value: ")
  (let  ((number (string-to-number value)))
    (make-variable-buffer-local varname)
    (if (and (= 0 number) (not (string-equal "0" value)))
        (set-variable varname value)
      (set-variable varname number))))

(defun ds/align-repeat (start end regexp)
  "Repeat alignment from START to END with respect to the given REGEXP."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
		(concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun ds/toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (while (search-forward "_" end t)
              (replace-match ""))
            (downcase-region start (1+ start)))
        (goto-char (1+ start))
        (while (re-search-forward "\\([A-Z]\\)" end t)
          (replace-match "_\\1"))
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun ds/find-eslint-executable ()
  "Find an executable for eslint preferring one in a `node_modules' folder in or above the current directory."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint-local (and root
                            (expand-file-name "node_modules/eslint/bin/eslint.js"
                                              root)))
         (eslint-system (executable-find "eslint")))
    (if (and (stringp eslint-local)
             (file-executable-p eslint-local))
        eslint-local
      eslint-system)))

(defun ds/open-next-line (count)
  "Open COUNT lines after the current one."
  (interactive "p")
  (end-of-line)
  (open-line count)
  (forward-line count)
  (indent-according-to-mode))

(defun ds/open-previous-line (count)
  "Open COUNT new line before the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line count)
  (indent-according-to-mode))


(provide 'ds-util)
;;; ds-util.el ends here
