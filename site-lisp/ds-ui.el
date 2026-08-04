;;; ds-ui.el --- emacs ui customizations       -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul B Davis

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

;; packages and settings for UI configuration

;;; Code:


(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'subword-minor-mode)
  (diminish 'eldoc-minor-mode)
  (diminish 'subword-mode)
  (diminish 'eldoc-mode))


(use-package multiple-cursors
  :ensure t
  :bind (("M-j" . mc/mark-next-like-this-symbol)))

(use-package vertico
  :ensure t
  :custom ((vertico-resize t))
  :bind (:map vertico-map
              ("C-e" . vertico-insert))
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package avy
  :ensure t
  :custom ((avy-keys '(?t ?n ?h ?e ?s ?o ?a ?i ?g ?y)))
  :bind (:map goto-map
              ("j" . avy-goto-word-0)
              ("M-j" . avy-goto-word-1)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-matching-styles '(orderless-flex
                               orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp)))

(use-package emacs
  :init
  (defun ds/get-buffer-side(buf alist)
    (let ((side 'bottom))
      (if (> (frame-pixel-width) (* 1.5 (frame-pixel-height)))
              (setq side 'right))
      (display-buffer-in-side-window buf `((side . ,side) (window-width . 0.5) (window-height . 0.5)))))
  :custom
  (display-buffer-base-action '(ds/get-buffer-side)))

(provide 'ds-ui)
;; ds-ui.el ends here
