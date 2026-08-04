;;; ds-basic.el --- basic emacs customizations       -*- lexical-binding: t; -*-

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

;; most basic emacs settings

;;; Code:

;; performance stuff (mainly for lsp-mode)
(setq gc-cons-threshold 12800000)
(setq read-process-output-max (* 4 1024 1024)) ; 4MiB

;; basic settings
(setq inhibit-startup-message t)
(setq-default create-lockfiles nil)

;; minimal UI decoration
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set font (and window borders on macos
(if (eq system-type 'darwin)
    (dolist (var '((font . "Monospace-12") (undecorated-round . t)))
      (add-to-list 'default-frame-alist var))
  (add-to-list 'default-frame-alist '(font . "Monospace-8")))

;; setup backup and temp file directories
(defvar ds/backup-directory
  (expand-file-name "tmp/backups" user-emacs-directory)
  "Where backups go.")

(defvar ds/autosave-directory
  (expand-file-name "tmp/autosave" user-emacs-directory)
  "Where autosaves go.")

(make-directory ds/backup-directory t)
(make-directory ds/autosave-directory  t)

(setq backup-by-copying t
      backup-directory-alist `((".*" .  ,ds/backup-directory))
      auto-save-file-name-transforms `((".*"  ,ds/autosave-directory t))
      auto-save-list-file-prefix  ds/autosave-directory
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; separate custom file
(defvar custom-file-location
  (expand-file-name "custom.el" user-emacs-directory)
  "File for customizations via \\[customize].")

(setq custom-file custom-file-location)
(if (file-readable-p custom-file-location)
    (progn
      (load custom-file)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

(show-paren-mode)

;; enable "advanced" commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'upcase-region 'disabled nil)

;; dired settings
(if (eq system-type 'darwin)
    (setq insert-directory-program "gls"
          dired-use-ls-dired t
          dired-listing-switches "-al --group-directories-first")
  (setq dired-listing-switches "-AFBhl  --group-directories-first"))

(defun ds/apply-lc-collate (wrapped-fun &rest args)
  "Set the env var `LC_COLLATE' to `C' and then run WRAPPED-FUN with ARGS."
  (let ((process-environment (copy-sequence process-environment)))
    (add-to-list 'process-environment "LC_COLLATE=C" nil 'string-equal)
    (apply wrapped-fun args)))

(advice-add 'dired-insert-directory :around #'ds/apply-lc-collate)

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete))

(defun ds/go-ts-mode-setup ()
  (setq go-ts-mode-indent-offset 4))
(defun ds/set-js-lsp-indent ()
  "Setup indent for javascipt LSP."
  (setq indent-tabs-mode nil))
(defun ds/set-js-lsp-indent ()
  "Setup indent for json LSP."
  (setq-local js-indent-level 2))

(add-hook 'go-ts-mode-hook 'ds/go-ts-mode-setup)
(add-hook 'js-ts-mode-hook 'ds/set-js-lsp-indent)
(add-hook 'json-ts-mode 'ds/set-json-lsp-indent)

(add-to-list 'auto-mode-alist '("PKGBUILD$" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("zshenv$" . sh-mode))
(add-to-list 'auto-mode-alist '("zprofile$" . sh-mode))

;; set mark for accidental scroll fixing
(add-function :before (symbol-function 'scroll-down-command) #'push-mark)
(add-function :before (symbol-function 'scroll-up-command) #'push-mark)

(defvar tramp-ssh-controlmaster-options
  (concat
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes "
   "-o HostKeyAlgorithms=+ssh-rsa -o PubkeyAcceptedKeyTypes=+ssh-rsa"))

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(global-auto-revert-mode)
(global-subword-mode)
(winner-mode)

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

;; enable editorconfig variables
(editorconfig-mode)

(provide 'ds-basic)
;; ds-basic.el ends here
