;;; init.el --- dangersalad emacs init               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Paul B Davis

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

;; Init for Emacs.  I used an org mode setup for a while, but really,
;; who is reading my Emacs config??

;;; Code:

(setq load-path (cons "~/.emacs.d/lib" load-path))
(setq load-path (cons "~/.emacs.d/packages" load-path))

(require 'package-loader)

(use-package ds-basic)
(use-package ds-theme)
(use-package ds-util
  :commands (ds/find-eslint-executable ds/get-zenburn-color)
  :bind (("C-c \\" . ds/indent-buffer)
	     ("C-c _" . ds/toggle-camelcase-underscores)
	     ("C-o" . ds/open-next-line)
	     ("M-o" . ds/open-previous-line)))

;; misc packages for general usability
(use-package adaptive-wrap
  :ensure
  :functions adaptive-wrap-prefix-mode
  :init
  (defvar adaptive-wrap-extra-indent 6)
  (defun ds/wrap-on-visual-line-mode ()
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  :hook (visual-line-mode . ds/wrap-on-visual-line-mode))

(use-package dired-subtree
  :ensure
  :bind (:map dired-mode-map
              ("i" . dired-subtree-t))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package multiple-cursors
  :ensure
  :bind (("M-j" . mc/mark-next-like-this-symbol)))

(use-package direnv
  :ensure
  :demand
  :config
  (direnv-mode)
  :hook ((eshell-directory-change . direnv-update-directory-environment)))

;; syntax checking
(use-package flycheck
  :ensure
  :demand
  :hook ((flycheck-mode . ds/use-eslint-from-node-modules))
  :custom ((flycheck-emacs-lisp-load-path 'inherit)
           (flycheck-display-errors-delay 0.4))
  :config
  ;; disable jshint
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; set eslint executable for flycheck to use
  (defun ds/use-eslint-from-node-modules ()
    (setq-local flycheck-javascript-eslint-executable (ds/find-eslint-executable)))
  
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode))

;; ivy/counsel setup
(use-package flx
  :ensure)

(use-package smex
  :ensure
  :defines smex-save-file
  :config
  (smex-initialize))

;; customize the colors
(defun ds/custom-ivy-faces ()
  "Set custom colors for the ivy completion minibuffer."
  (set-face-attribute 'ivy-subdir nil :foreground (ds/get-zenburn-color "blue-1") :background nil :weight 'bold)
  (set-face-attribute 'ivy-remote nil :foreground (ds/get-zenburn-color "red-1") :background nil :weight 'bold)
  (set-face-attribute 'ivy-current-match nil :foreground nil :background (ds/get-zenburn-color "bg+3") :box (ds/get-zenburn-color "blue") :underline nil)
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :background nil :box (ds/get-zenburn-color "green-1") :underline nil)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background nil :box (ds/get-zenburn-color "green-1") :underline nil)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background nil :box (ds/get-zenburn-color "red-1") :underline nil)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background nil :box (ds/get-zenburn-color "yellow-1") :underline nil))

(use-package ivy
  :ensure
  :diminish (ivy-mode . "")
  :bind (("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-'" . ivy-avy)
         ("C-e" . ivy-alt-done)
         :map read-expression-map)
  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; recursive minibuffer
  (setq enable-recursive-minibuffers t)
  ;; count display
  (setq ivy-count-format "(%d/%d) ")
  ;; wrap
  (setq ivy-wrap t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  (ivy-mode 1)
  (ds/custom-ivy-faces))

(use-package ivy-hydra
  :ensure)

(use-package counsel
  :ensure
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history)))

;; project management
(use-package projectile
  :ensure
  :after ivy
  :bind-keymap ("C-c C-p" . projectile-command-map)
  :init
  (defvar projectile-remember-window-configs t)
  (defvar projectile-mode-line
    '(:eval
      (if (file-remote-p default-directory)
          " NoProj"
        (format " Proj[%s]"
                (projectile-project-name)))))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure
  :after projectile
  :init
  ;; shim for "older" ivy
  (defvar ivy-highlight-grep-commands nil
    "List of counsel grep-like commands.")
  :config
  (counsel-projectile-mode))

;; git porcelean
(use-package magit
  :ensure
  :bind (:map magit-mode-map
              ([remap previous-line] . magit-previous-line)
              ([remap next-line] . magit-next-line))
  :config
  (setq magit-merge-arguments '("--no-ff"))
  (setq global-magit-file-mode        t
        magit-log-highlight-keywords  t
        magit-diff-highlight-keywords t)
  
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-timemachine
  :ensure)

(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install))

;; popup display macro
(defmacro ds/popup-thing-display-settings (BUFFER-NAME SIDE &optional SLOT SIZE)
  "Make a popup buffer on SIDE for BUFFER-NAME."
  `(add-to-list 'display-buffer-alist
                '(,(concat "\\`" (regexp-quote BUFFER-NAME) "\\'")
                  (display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (side            . ,SIDE)
                  ,(if SLOT `(slot            . ,SLOT))
                  (reusable-frames)
                  (inhibit-switch-frame . t)
                  ,(if SIZE
                       (if (or (equal SIDE 'top)
                               (equal SIDE 'bottom))
                           `(window-height . ,SIZE)
                         `(window-width   . ,(if (< SIZE 1) SIZE
                                               `(lambda (win)
                                                  (if (or (< (window-width win) ,SIZE)
                                                          (not (or (window-in-direction 'above win t)
                                                                   (window-in-direction 'below win t))))
                                                      (ds/set-window-column-width ,SIZE win))))))))))


;; compilation settings
(use-package compile
  :config
  (define-key compilation-mode-map (kbd "q") #'delete-window)
  (ds/popup-thing-display-settings "*compilation*" right 2 104)

  (setq compilation-finish-functions
        '((lambda (buf str)
            (message "compilation %s" str)
            (if (eq 0 (string-match-p "^finished$" str))
                (let ((project-root (if (projectile-project-p) (projectile-project-root) nil)))
                  (run-at-time
                   2 nil 'delete-windows-on
                   (get-buffer-create "*compilation*"))
                  (if project-root
                      (run-at-time
                       2.01 nil 'projectile-vc project-root)))))))

  (setq compilation-scroll-output t))

(provide 'init)
;;; init.el ends here
