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
(setq load-path (cons "~/.emacs.d/lib/emacs-org-dnd" load-path))
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

;; highlight parens in emacs lisp mode
(use-package highlight-parentheses
  :ensure
  :diminish highlight-parentheses-mode
  :hook (emacs-lisp-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-background-colors
        `(,(ds/get-zenburn-color "bg-2")
          ,(ds/get-zenburn-color "bg-1")
          ,(ds/get-zenburn-color "bg-05")
          ,(ds/get-zenburn-color "bg+05")
          ,(ds/get-zenburn-color "bg+1")
          ,(ds/get-zenburn-color "bg+2")
          ,(ds/get-zenburn-color "bg+3")
          ,(ds/get-zenburn-color "fg-1")))
  (setq hl-paren-colors
        `(,(ds/get-zenburn-color "red-2")
          ,(ds/get-zenburn-color "green")
          ,(ds/get-zenburn-color "orange")
          ,(ds/get-zenburn-color "blue")
          ,(ds/get-zenburn-color "yellow")
          ,(ds/get-zenburn-color "cyan")
          ,(ds/get-zenburn-color "magenta")
          ,(ds/get-zenburn-color "fg+1"))))

;; commenting
(use-package evil-nerd-commenter
  :ensure
  :bind (("C-c C-/ C-/" . evilnc-comment-or-uncomment-lines)
         ("C-c C-/ C-l" . evilnc-comment-or-uncomment-to-the-line)
         ("C-c C-/ C-c" . evilnc-copy-and-comment-lines)
         ("C-c C-/ C-p" . evilnc-comment-or-uncomment-paragraphs)
         ("C-c C-_ C-_" . evilnc-comment-or-uncomment-lines)
         ("C-c C-_ C-l" . evilnc-comment-or-uncomment-to-the-line)
         ("C-c C-_ C-c" . evilnc-copy-and-comment-lines)
         ("C-c C-_ C-p" . evilnc-comment-or-uncomment-paragraphs)))

;; some basic modes for files I work on
(use-package nginx-mode
  :ensure)
(use-package json-mode
  :ensure)
(use-package dockerfile-mode
  :ensure)
(use-package yaml-mode
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; sql stuff (postgres by defualt)
(require 'sql)
(defun ds/postgresql-highlight ()
  "Setup sql for postgres."
  (sql-mode)
  (sql-highlight-postgres-keywords))

(add-to-list 'auto-mode-alist
             '("\\.sql$" . ds/postgresql-highlight))

(use-package sql-indent
  :ensure
  :hook (sql-mode . sqlind-minor-mode))

;; rest client
(use-package restclient
  :ensure
  :config
  (ds/popup-thing-display-settings "*HTTP Response*" left 0 0.25))

;; golang
(use-package go-mode
  :ensure
  :mode ("\\go.mod\\'" . fundamental-mode)
  :hook ((before-save . gofmt-before-save)
         (after-save . flycheck-buffer)))

;; javascript
(defun ds/eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  (let ((eslint (ds/find-eslint-executable)))
    (if eslint
        (progn (call-process eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
               (revert-buffer t t t))
      (message "ESLint not found."))))
(defun ds/setup-eslint-fix ()
  "Setup eslint fixing."
  (add-hook 'after-save-hook #'ds/eslint-fix))

(use-package js
  :hook ((js-mode . ds/setup-eslint-fix))
  :config
  (setq js-indent-level 2))

;; html/web
(use-package web-mode
  :ensure
  :hook ((web-mode . ds/setup-eslint-fix))
  :mode ("\\.html\\'")
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-part-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

;; protobufs
(use-package protobuf-mode
  :ensure
  :hook (protobuf-mode . ds/protobuf-setup)
  :config
  (defun ds/protobuf-setup ()
    (flycheck-define-checker protobuf-protoc
      "A modified protobuf syntax checker using the protoc compiler.

          See URL `https://developers.google.com/protocol-buffers/'."
      :command ("protoc" "--error_format" "gcc"
                (eval (concat "--java_out=" (flycheck-temp-dir-system)))
                ;; include the directory with the file and subdirs I use in my projects
                (eval (concat "--proto_path=" (file-name-directory (buffer-file-name))))
                (eval (concat "--proto_path=" (file-truename (concat (file-name-directory (buffer-file-name)) "lib/proto"))))
                (eval (concat "--proto_path=" (file-truename (concat (file-name-directory (buffer-file-name)) "third_party"))))
                source-inplace)
      :error-patterns
      ((info line-start (file-name) ":" line ":" column
             ": note: " (message) line-end)
       (error line-start (file-name) ":" line ":" column
              ": " (message) line-end)
       (error line-start
              (message "In file included from") " " (file-name) ":" line ":"
              column ":" line-end))
      :modes protobuf-mode
      :predicate buffer-file-name)))

;; org-mode

;; expand logbook on org all expand
(defun ds/expand-logbook-drawer ()
  "Expand the closest logbook drawer."
  (interactive)
  (search-forward ":LOGBOOK:")
  (org-cycle))

(defun ds/org-logbook-cycle-hook (ds/drawer-curr-state)
  "When the DS/DRAWER-CURR-STATE is \"all\", open up logbooks."
  (interactive)
  (message "State changed")
  (when (eq ds/drawer-curr-state "all")
    
    (ds/expand-logbook-drawer)))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :pin org
  :custom-face
  (org-mode-line-clock
   ((t (:foreground nil :background nil :underline nil :box nil))))
  :hook (org-cycle . ds/org-logbook-cycle-hook)
  :init
  (defvar org-directory "~/org" "Directory for org files.")
  (defvar org-time-clocksum-format "%d:%.02d")
  (defvar org-clock-idle-time 15)
  (defvar org-clock-mode-line-total 'current)
  (defvar org-clock-into-drawer "LOGBOOK")
  (defvar org-duration-format '(("h" . t) (special . 2)))
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (defvar org-clock-persist t)
  ;; Resume clocking task on clock-in if the clock is open
  (defvar org-clock-in-resume t)
  ;; Do not prompt to resume an active clock, just resume it
  (defvar org-clock-persist-query-resume nil)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
  ;; with 0:00 duration
  (defvar org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (defvar org-clock-out-when-done t)
  ;; Enable auto clock resolution for finding open clocks
  (defvar org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (defvar org-clock-report-include-clocking-task t)
  (defvar org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  :config
  (condition-case nil
      ;; make the org dir if it is not there already
      (make-directory org-directory t)
    (error nil))
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-src-window-setup 'current-window)
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; use pretty things for the clocktable
  (setq org-pretty-entities t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@)" "|" "WILL-NOT-IMPLEMENT(k@)" "DONE(d)")
          (sequence "BUG(b)" "RESOLVING(r!)" "|" "NON-ISSUE(n@)" "PATCHED(p)")))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (ditaa . t))))

(use-package org-bullets
  :ensure
  :after org
  :hook (org-mode . org-bullets-mode))


;; dnd
(require 'ox-dnd)

(provide 'init)
;;; init.el ends here
