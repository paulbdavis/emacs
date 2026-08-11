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

(use-package avy
  :ensure t
  :custom ((avy-keys '(?t ?n ?h ?e ?s ?o ?a ?i ?g ?y)))
  :bind (:map goto-map
              ("j" . avy-goto-word-0)
              ("M-j" . avy-goto-word-1)))



;; setup window and frame display settings
(use-package emacs
  :init
  (defun ds/display-buffer-side(buf alist)
    (let ((side 'bottom))
      (if (> (frame-pixel-width) (* 1.5 (frame-pixel-height)))
          (setq side 'right))
      (display-buffer-in-side-window buf `((side . ,side) (window-width . 0.5) (window-height . 0.5)))))
  (defun ds/display-buffer-direction(buf alist)
    (let ((side 'bottom))
      (if (> (frame-pixel-width) (* 1.5 (frame-pixel-height)))
          (setq side 'right))
      (display-buffer-in-direction buf `((direction . ,side) (window-width . 0.5) (window-height . 0.5)))))
  :custom
  (display-buffer-base-action '(ds/display-buffer-direction))
  :config
  ;; open other files in frames instead of windows
  (define-key global-map (kbd "C-x 4") #'ctl-x-5-prefix)
  (setq display-buffer-alist '(("^magit:" . (display-buffer-full-frame))
                               ("^magit-log:" . (display-buffer-pop-up-frame))
                               ("*info*" . (display-buffer-pop-up-frame))
                               ("\\*vterm" . ((display-buffer-reuse-window display-buffer-pop-up-frame)
                                              . ((inhibit-same-window . t)
                                                 (reusable-frames . t)))))))

;; setup completion settings (emacs 31+)
(use-package minibuffer
  :init
  (defun ds/minibuffer-truncate-lines ()
    "Keep minibuffer lines unwrapped."
    (setq truncate-lines t))

  (defun ds/flex-noinsert-try-completion (string table pred point)
    "Flex `try-completion' that never auto-extends the input on TAB.

The stock `flex' completion style does two jobs: it filters
candidates by fuzzy (subsequence) match, and its `try-completion'
merges the surviving candidates, inserting their common expansion
into the buffer.  With `tab-always-indent' set to `complete' that
merge means TAB silently types a candidate (often a far, wrong one)
*before* the *Completions* list is shown.  Eglot's own
`eglot--dumb-flex' avoids the merge but gives no relevance sorting.

This wrapper keeps flex's filtering and scoring (so prefix matches
sort first, fuzzy ones last) but suppresses the merge:

  - no candidates           -> nil   (no match)
  - exactly one candidate   -> complete it fully (TAB still finishes
							   a unique completion)
  - two or more candidates  -> return STRING unchanged, so TAB only
							   pops the *Completions* list and lets
							   you pick, inserting nothing.

STRING, TABLE, PRED and POINT are the usual `try-completion' args."
    (let ((all (completion-flex-all-completions string table pred point)))
	  (cond
	   ((null all) nil)
	   ((= (safe-length all) 1)
	    (let ((sole (car all)))
		  (if (string= sole string) t (cons sole (length sole)))))
	   (t (cons string point)))))
  
  :bind (:map minibuffer-visible-completions-up-down-map
              ("C-n" . minibuffer-next-completion)
              ("C-p" . minibuffer-previous-completion)
              :map minibuffer-mode-map
              ("C-e" . minibuffer-complete))
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (minibuffer-setup . ds/minibuffer-truncate-lines))
  :custom
  (completion-auto-help t)
  (completion-auto-select t)
  (completion-eager-update t)
  (completion-eager-display t)
  (minibuffer-visible-completions 'up-down)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completion-styles '(partial-completion flex initials))
  (completion-category-overrides '((eglot-capf (styles flex-noinsert))))
  (completions-format 'one-column)
  (completions-max-height 30)
  (completions-sort 'historical)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibufer-prompt))
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  :config
  ;; Register the `flex-noinsert' style: same filtering/sorting as
  ;; `flex', but with the wrapper above as its try function.
  (add-to-list 'completion-styles-alist
			   '(flex-noinsert
			     ds/flex-noinsert-try-completion
			     completion-flex-all-completions
			     "Flex matching that never extends input on TAB."))

  ;; Reuse flex's metadata tweak so *Completions* sorts by flex score.
  (put 'flex-noinsert 'completion--adjust-metadata
	   'completion--flex-adjust-metadata))



(provide 'ds-ui)
;; ds-ui.el ends here
