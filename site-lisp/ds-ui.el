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

(use-package frames-only-mode
  :ensure t
  :custom ((frames-only-mode-kill-frame-when-buffer-killed-buffer-list
            '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*" "*HTTP Headers*" "*Compilation*")))
  :config
  (frames-only-mode)
  (with-eval-after-load 'vterm
    (defun ds/frames-only-mode-kill-frame-if-current-buffer-is-vterm ()
      "Kill frames as well when certain buffers are closed.

Only if there is only a single window in the frame, helps stop some
packages spamming frames."
      (when (and (one-window-p)
                 (eq major-mode 'vterm-mode))
        (delete-frame)))


    (defun ds/frames-only-mode-advice-delete-vterm-frame-on-bury (orig-fun &rest args)
      "Delete the frame when burying certain buffers.

Only if there are no other windows in the frame, and if the buffer is in frames-only-mode-kill-frame-when-buffer-killed-buffer-list."
      ;; Store the buffer name now because we can't get it after burying the buffer
      (let ()
        (apply orig-fun args)
        (when (and (one-window-p)
                   (eq major-mode 'vterm-mode))
          (delete-frame))))

    (add-hook 'kill-buffer-hook #'ds/frames-only-mode-kill-frame-if-current-buffer-is-vterm)
    (advice-add #'bury-buffer :around #'ds/frames-only-mode-advice-delete-vterm-frame-on-bury)))            

(provide 'ds-ui)
;; ds-ui.el ends here
