;;; ds-basic.el --- basic emacs customizations       -*- lexical-binding: t; -*-

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

(require 'diminish)
(require 'use-package)

(setq inhibit-startup-message t)
(setq-default create-lockfiles nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(font . "Monospace-10"))

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

(put 'narrow-to-region 'disabled nil)

(defvar erc-hide-list '("JOIN" "PART" "QUIT"))

(use-package ediff
  :commands (ediff-setup-windows-plain)
  :defines (ediff-setup-windows-plain)
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

;; close ansi term when exiting
(defun ds/ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc))
                              (if (not (= (length (window-list)) 1))
                                  (delete-window)))))))

(add-hook 'term-mode-hook #'ds/ansi-term-handle-close)

;; more highlighting in sh-mode
(defun sh-script-extra-font-lock-match-var-in-double-quoted-string (limit)
  "Search for variables in double-quoted strings with LIMIT."
  (let (res)
    (while
        (and (setq res (progn (if (eq (get-byte) ?$) (backward-char))
                              (re-search-forward
                               "[^\\]\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\|[[:digit:]]+\\)"
                               limit t)))
             (not (eq (nth 3 (syntax-ppss)) ?\")))) res))

(defvar sh-script-extra-font-lock-keywords
  '((sh-script-extra-font-lock-match-var-in-double-quoted-string
     (2 font-lock-variable-name-face prepend))))

(defun sh-script-extra-font-lock-activate ()
  "Activate extra font locking for shell scripts."
  (interactive)
  (font-lock-add-keywords nil sh-script-extra-font-lock-keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'sh-mode-hook 'sh-script-extra-font-lock-activate)

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
   "-o ControlMaster=auto -o ControlPersist=yes"))

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(global-auto-revert-mode)
(global-subword-mode)
(winner-mode)
(diminish 'abbrev-mode)
(diminish 'subword-minor-mode)
(diminish 'eldoc-minor-mode)
(diminish 'subword-mode)
(diminish 'eldoc-mode)


(setq dired-listing-switches "-lha --group-directories-first")

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

(provide 'ds-basic)
;;; ds-basic.el ends here
