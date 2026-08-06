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
  "Indent entire buffer using `indent-region'."
  (interactive)
  (save-excursion
    (push-mark (point))
    (push-mark (point-max) nil t)
    (goto-char (point-min))
    (indent-region (region-beginning) (region-end))))

(defun ds/align-repeat (start end regexp)
  "Repeat alignment from START to END with respect to the given REGEXP."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
		        (concat "\\(\\s-*\\)" regexp) 1 1 t))

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

(defun ds/toggle-mode-line ()
  "Toggle the modeline on and off."
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)) )
  (redraw-display))

(provide 'ds-util)
;;; ds-util.el ends here
