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
(use-package ds-theme
  :commands (ds/get-zenburn-color))

(use-package ds-util
  :commands (ds/find-eslint-executable ds/get-zenburn-color)
  :bind (("C-c \\" . ds/indent-buffer)
	     ("C-c _" . ds/toggle-camelcase-underscores)
	     ("C-o" . ds/open-next-line)
	     ("M-o" . ds/open-previous-line)))

(use-package ds-eshell)

(use-package vterm
  :ensure t
  :commands (vterm)
  :after ds-theme
  :config
  (defun ds/vterm (&optional name)
    (interactive "MName: ")
    (if (< 0 (length name))
        (if (get-buffer name)
            (switch-to-buffer name)
          (vterm name))
      (vterm))))

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
  :defines dired-subtree-toggle
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle))
  :config
  (setq dired-subtree-use-backgrounds nil))

(setq dired-listing-switches "-AFBhl  --group-directories-first")

(defun ds/apply-lc-collate (wrapped-fun &rest args)
  "Set the env var `LC_COLLATE' to `C' and then run WRAPPED-FUN with ARGS."
  (let ((process-environment (copy-sequence process-environment)))
    (add-to-list 'process-environment "LC_COLLATE=C" nil 'string-equal)
    (apply wrapped-fun args)))

(advice-add 'dired-insert-directory :around #'ds/apply-lc-collate)

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
  :defines flycheck-add-mode
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
         ;; ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-'" . ivy-avy)
         ("C-e" . ivy-alt-done)
         :map read-expression-map)
  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; recursive minibuffer
  ;; (setq enable-recursive-minibuffers t)
  ;; count display
  (setq ivy-count-format "(%d/%d) ")
  ;; wrap
  (setq ivy-wrap t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist '((counsel-ag . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))

  (setq ivy-display-functions-alist '())
  
  (ivy-mode 1)
  (ds/custom-ivy-faces))

(use-package ivy-hydra
  :ensure)

(use-package ivy-posframe
  :ensure
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          (completion-at-point . ivy-posframe-display-at-point)
          (ivy-completion-in-region . ivy-posframe-display-at-point)
          (t               . nil)))
  (ivy-posframe-mode 1))

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
  :bind-keymap ("C-c p" . projectile-command-map)
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
  
  (setq magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident nil)
          ("Branch" 25 magit-repolist-column-branch nil)
          ("Version" 40 magit-repolist-column-version nil)
          ("B<U" 3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U" 3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path" 99 magit-repolist-column-path nil)))
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
(use-package markdown-mode
  :ensure)
(use-package systemd
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
  (ds/popup-thing-display-settings "*HTTP Response*" left 0 0.25)
  (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

;; golang
(use-package go-mode
  :ensure
  :mode ("\\go.mod\\'" . fundamental-mode))

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

;; make a vue-mode that is just web mode with a different name
(define-derived-mode vue-mode web-mode "VueJS")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook #'ds/setup-eslint-fix)
(flycheck-add-mode 'javascript-eslint 'vue-mode)

;; protobufs

(setq c-default-style "linux"
      c-basic-offset 4)

(use-package protobuf-mode
  :ensure
  :hook (protobuf-mode . ds/protobuf-setup)
  :config
  (defun ds/protobuf-setup ()
    ;; add a flycheck checker
    (flycheck-define-checker protobuf-protoc
      "A modified protobuf syntax checker using the protoc compiler.

          See URL `https://developers.google.com/protocol-buffers/'."
      :command ("protoc" "--error_format" "gcc"
                (eval (concat "--java_out=" (flycheck-temp-dir-system)))
                ;; include the directory with the file and subdirs I use in my projects
                (eval (concat "--proto_path=" (file-name-directory (buffer-file-name))))
                (eval (concat "--proto_path=" (file-truename (concat (file-name-directory (buffer-file-name)) "lib/proto"))))
                (eval (concat "--proto_path=" (file-truename (concat (file-name-directory (buffer-file-name)) "third_party"))))
                (eval (concat "--proto_path=" (file-truename (concat (file-name-directory (buffer-file-name)) ".."))))
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

;; (setq load-path (cons "~/dev/emacs/lsp-mode" load-path))

;; LSP stuff
(use-package yasnippet
  :ensure)

(use-package lsp-mode
  :ensure
  :demand
  :defines (lsp-gopls-hover-kind lsp-gopls-env)
  ;; :disabled
  :commands (lsp lsp-deferred)
  :hook ((go-mode . ds/go-mode-lsp)
         (js-mode . ds/js-mode-lsp)
         (vue-mode . ds/js-mode-lsp))
  :init
  (flycheck-define-checker go-golint-solo
    "A golang style checker using golint that does not define any next checkers.

See URL `https://github.com/golang/lint'."
    :command ("golint" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes go-mode)
  
  (add-to-list 'flycheck-checkers 'go-golint-solo)
  
  (defun ds/go-mode-lsp ()
    "Enable `lsp-mode' in `go-mode' unless there is an import \"C\" statement. Also set up other stuff."

    (save-excursion
      (with-current-buffer (current-buffer)
        (if (search-forward "import \"C\"" nil t 1)
            (setq-local lsp-gopls-env   ; set env for cgo
                        #s(hash-table data ("CGO_ENABLED" "1" "GOBIN" ""))))))
    

    ;; experimental options not in lsp mode yet
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" t t)))
    
    (lsp)
    ;; (flycheck-add-next-checker 'lsp-ui 'go-golint 'append)
    (setf (flycheck-checker-get 'lsp-ui 'next-checkers) (list 'go-golint-solo))
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  
  (defun ds/js-mode-lsp ()
    "Enable `lsp-mode' in `go-mode' unless there is an import \"C\" statement. Also set up other stuff."

    (lsp)
    ;; (flycheck-add-next-checker 'lsp-ui 'go-golint 'append)
    (setf (flycheck-checker-get 'lsp-ui 'next-checkers) (list 'javascript-eslint))
    ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
    ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
    )
  

  :config
  (setq lsp-prefer-flymake nil)

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "gopls2")
  ;;                   :major-modes '(go-mode)
  ;;                   :server-id 'gopls))

  (setq lsp-gopls-hover-kind "FullDocumentation")

  (use-package lsp-ui
    :ensure
    :commands lsp-ui-mode
    :hook ((lsp-mode-hook . lsp-ui-mode))
    :config
    
    (setq lsp-ui-doc-position 'bottom)
    (setq lsp-ui-doc-delay 1)
    ;; (setq lsp-ui-doc-use-childframe nil)
    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-delay 1)
    (set-face-background 'lsp-ui-doc-background (ds/get-zenburn-color "bg"))
    (set-face-background 'lsp-ui-doc-header (ds/get-zenburn-color "bg"))
    (set-face-foreground 'lsp-ui-doc-header (ds/get-zenburn-color "fg"))
    ))
;; (setq load-path (cons "~/dev/emacs/eglot" load-path))

(use-package eglot
  :ensure
  :disabled
  :hook ((go-mode . eglot-ensure)
         (vue-mode . eglot-ensure)
         (js-mode . eglot-ensure))
  :config
  ;; (add-to-list 'eglot-server-programs '(go-mode . ("bingo")))
  (add-to-list 'eglot-server-programs '(go-mode . ("bingo2" "-disable-func-snippet")))
  ;; (add-to-list 'eglot-server-programs '(go-mode . ("gopls2")))
  (add-to-list 'eglot-server-programs '(vue-mode . ("vls")))
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))

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
  (defvar org-agenda-directory "~/org/agenda" "Directory for org files.")
  :config
  (setq org-agenda-file-regexp "\\([^.].*\\.org\\)\\|\\([0-9]+\\)")
  (defun org-agenda-reload ()
    "Reset org agenda files by rescanning the org directory."
    (interactive)
    (setq org-agenda-files (directory-files-recursively org-agenda-directory "\\.org\\|[0-9]\\{8\\}"))
    (setq org-refile-targets '((org-agenda-files . (:level . 1)))))

  (org-agenda-reload)
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

;; chordpro mode

(defvar chordpro-font-lock-defaults
  '((("\\(\\[[^]]*\\]\\)" . font-lock-string-face)
     ("^\\(#.*\\)" . font-lock-comment-face)
     ("\\({subtitle[^}]*}\\)" . font-lock-type-face)
     ("\\({title[^}]*}\\)" . font-lock-keyword-face)
     ("\\({[^}]*}\\)" . font-lock-variable-name-face))))


(define-derived-mode chordpro-mode text-mode "Chordpro"
  "Major mode for editing Chordpro files.
     Special commands:
     \\{chordpro-mode-map}"
  (setq font-lock-defaults chordpro-font-lock-defaults)
  (auto-fill-mode -1))

(add-to-list 'auto-mode-alist '("\\.pro$" . chordpro-mode))
(add-to-list 'auto-mode-alist '("\\.chopro$" . chordpro-mode))
(add-to-list 'auto-mode-alist '("\\.chordpro$" . chordpro-mode))


(defvar ds/exwm-enable nil
  "Set to t to use exwm.")

(if (file-exists-p "~/.emacs.d/local.el")
    (load-file "~/.emacs.d/local.el"))

(if ds/exwm-enable
    (progn
      (setq load-path (cons "~/.emacs.d/lib/exwm" load-path))
      (setq load-path (cons "~/.emacs.d/lib/eosd" load-path))

      (use-package xelb
        :ensure t)

      (use-package pass
        :ensure t
        :config
        (use-package password-store-otp
          :ensure t
          :init
          (defun ds/password-store-get-otp (record)
            (interactive (list (completing-read "TOTP entry: " (password-store-list))))
            (password-store-otp-token-copy record))))
      
      (use-package exec-path-from-shell
        :ensure t
        :demand
        :config
        (progn
          (message "setting up exec path")
          (exec-path-from-shell-initialize)
          (message "set up exec path")))

      (use-package exwm
        :demand
        :init
        (defun ds/exwm-set-name ()
          ;; (message "class: %s, instance: %s, title: %s, state: %s, type: %s" exwm-class-name exwm-instance-name exwm-title exwm-state exwm-window-type)
          (exwm-workspace-rename-buffer exwm-class-name))
        (defvar ds/exwm-previous-workspace nil
          "Stores previous workspace when switching in exwm")
        (defun ds/lock-screen (&rest _)
          (interactive)
          (start-process "" nil "gpg-connect-agent" "killagent" "/bye")
          (start-process "" nil "slock"))

        (defun ds/exwm-refresh-notification-buffer (&rest _)
          (if (get-buffer-window (get-buffer "*notifications*"))
              (eosd-mode-create-or-update-buffer)))

        (defun ds/exwm-notification-autopop (&rest _)
          "Popup the notification buffer without taking focus from the current window."
          (let ((currentbuffer (buffer-name)))
            (if (equal currentbuffer "*notifications*")
                (ds/exwm-refresh-notification-buffer)
              (progn (ds/exwm-eosd)
                     (with-current-buffer currentbuffer
                       (select-window (get-buffer-window (buffer-name)) t))))))
        ;; load eosd
        (require 'eosd)
        ;; customize notification faces
        (eval-after-load 'zenburn-theme
          (zenburn-with-color-variables
            (set-face-attribute 'eosd-heading-face nil :foreground zenburn-fg-1)
            (set-face-attribute 'eosd-title-face nil :foreground zenburn-green)
            (set-face-attribute 'eosd-datetime-face nil :foreground zenburn-blue)
            (set-face-attribute 'eosd-action-link-face nil :foreground zenburn-blue :background zenburn-bg+1 :box zenburn-blue :underline nil)
            (set-face-attribute 'eosd-delete-link-face nil :foreground zenburn-red :background zenburn-bg+1 :box zenburn-red :underline nil)
            (set-face-attribute 'eosd-text-mark-face nil :foreground zenburn-fg-1)))
        ;; start the notification service
        (eosd-start)
        
        :config
        ;; auto rename new X window buffers
        (add-hook 'exwm-update-class-hook #'ds/exwm-set-name)
        ;; hide the mode-line of floating X windows
        (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
        (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
        ;; 'C-s-n': Rename buffer
        (exwm-input-set-key (kbd "C-s-n") #'rename-buffer)
        ;; 'C-s-r': Reset
        (exwm-input-set-key (kbd "C-s-r") #'exwm-reset)
        ;; 'C-s-f': Toggle Fullscreen
        (exwm-input-set-key (kbd "C-s-f") #'exwm-layout-toggle-fullscreen)
        ;; do xinit stuff
        (start-process "" nil (concat user-emacs-directory "exwm/bin/xinitscript"))
        (start-process "" nil (concat user-emacs-directory "exwm/bin/wallpaper"))
        (start-process "" nil "compton")
        (start-process "" nil "pulseaudio")
        ;; disable flycheck for exwm buffers
        (add-hook 'exwm-mode-hook (lambda () (flycheck-mode -1)))
        (defmacro ds/popup-thing (NAME BUFFER &rest BODY)
          "Make a popup thing with function NAME buffer name BUFFER executing BODY to create."
          (let* ((delete-func-sym (intern (concat (symbol-name NAME) "--delete"))))
            `(progn
               (defun ,delete-func-sym (&rest _)
                 (let ((current-popup (get-buffer-window ,BUFFER)))
                   (if (and current-popup
                            (> (length (window-list)) 1))
                       (delete-window current-popup))))
               (add-function :before (symbol-function 'exwm-workspace-switch) #',delete-func-sym)
               (defun ,NAME ()
                 (interactive)
                 (let* ((win (selected-window))
                        (current-popup (or (get-buffer-window ,BUFFER t)
                                           (get-buffer-window ,(concat " " BUFFER) t)))
                        (popup-buf (or (get-buffer ,BUFFER)
                                       (get-buffer ,(concat " " BUFFER))))
                        (is-x-window (if popup-buf
                                         (equal 'exwm-mode (with-current-buffer popup-buf major-mode)))))
                   (if (equal win current-popup)
                       (delete-window current-popup)
                     (if current-popup
                         (select-window current-popup)
                       (if popup-buf
                           (progn
                             (if is-x-window
                                 (save-window-excursion
                                   (with-current-buffer popup-buf
                                     (exwm-workspace-move-window exwm-workspace--current exwm--id))))
                             (pop-to-buffer popup-buf))
                         (progn ,@BODY)))))))))
        (setq exwm-workspace-number 10)
        ;; set up bindings to switch to workspaces
        (dotimes (i 10)
          (let* ((switch-binding (kbd (format "s-%d" i)))
                 (move-binding (kbd (format "C-s-%d" i))))
            ;; use s-N to switch to a workspace number
            (exwm-input-set-key switch-binding
                                `(lambda ()
                                   (interactive)
                                   (exwm-workspace-switch-create ,i)))
            ;; use C-s-N to move the current window to a workspace
            (exwm-input-set-key move-binding
                                `(lambda ()
                                   (interactive)
                                   (exwm-workspace-move-window ,i)
                                   (select-frame-set-input-focus exwm-workspace--current)))))
        (defun ds/exwm-mark-previous (&rest _)
          "Save the current EXWM workspace index to `ds/exwm-previous-workspace'."
          (setq ds/exwm-previous-workspace exwm-workspace-current-index))

        (defun ds/exwm-workspace-toggle ()
          "Switch back to the previously active EXWM workspace."
          (interactive)
          (exwm-workspace-switch ds/exwm-previous-workspace))
        ;; (remove-function (symbol-function 'exwm-workspace-switch) #'ds/exwm-mark-previous)
        (add-function :before (symbol-function 'exwm-workspace-switch) #'ds/exwm-mark-previous)

        ;; use s-tab to switch workspaces back and forth
        (exwm-input-set-key (kbd "<s-tab>") #'ds/exwm-workspace-toggle)

        ;; fix magit for this key
        (with-eval-after-load 'magit
          (defun ds/exwm-fix-magit-workspace-toggle ()
            (define-key magit-status-mode-map (kbd "<s-tab>") #'ds/exwm-workspace-toggle))
          (add-hook 'magit-status-mode-hook #'ds/exwm-fix-magit-workspace-toggle))
        ;; 's-SPC': Launch application
        (setq counsel-linux-app-format-function 'counsel-linux-app-format-function-name-only)
        (exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)
        ;; 's-r': Run shell command
        (exwm-input-set-key (kbd "s-r")
                            (lambda (command)
                              (interactive (list (read-shell-command "$ ")))
                              (start-process-shell-command command nil command)))
        (defun ds/adjust-window-leading-edge (delta dir)
          (let ((otherwin (window-in-direction dir))
                (otherdelta (* -1 delta)))
            (if otherwin
                (adjust-window-trailing-edge otherwin otherdelta (equal dir 'left)))))

        (defun ds/adjust-window-trailing-edge (delta dir)
          (adjust-window-trailing-edge (selected-window) delta (equal dir 'right)))

        (defun ds/exwm-window-resize--get-delta (delta default)
          (abs (or delta default)))

        (defun ds/exwm-window-grow-above (delta)
          (interactive "P")
          (ds/adjust-window-leading-edge (ds/exwm-window-resize--get-delta delta 5) 'above))

        (defun ds/exwm-window-shrink-above (delta)
          (interactive "P")
          (ds/adjust-window-leading-edge (* -1 (ds/exwm-window-resize--get-delta delta 5)) 'above))

        (defun ds/exwm-window-grow-below (delta)
          (interactive "P")
          (ds/adjust-window-trailing-edge (ds/exwm-window-resize--get-delta delta 5) 'below))

        (defun ds/exwm-window-shrink-below (delta)
          (interactive "P")
          (ds/adjust-window-trailing-edge (* -1 (ds/exwm-window-resize--get-delta delta 5)) 'below))

        (defun ds/exwm-window-grow-left (delta)
          (interactive "P")
          (ds/adjust-window-leading-edge (ds/exwm-window-resize--get-delta delta 10) 'left))

        (defun ds/exwm-window-shrink-left (delta)
          (interactive "P")
          (ds/adjust-window-leading-edge (* -1 (ds/exwm-window-resize--get-delta delta 10)) 'left))

        (defun ds/exwm-window-grow-right (delta)
          (interactive "P")
          (ds/adjust-window-trailing-edge (ds/exwm-window-resize--get-delta delta 10) 'right))

        (defun ds/exwm-window-shrink-right (delta)
          (interactive "P")
          (ds/adjust-window-trailing-edge (* -1 (ds/exwm-window-resize--get-delta delta 10)) 'right))

        (exwm-input-set-key (kbd "<C-s-up>") #'ds/exwm-window-grow-above)
        (exwm-input-set-key (kbd "<C-M-s-up>") #'ds/exwm-window-shrink-above)

        (exwm-input-set-key (kbd "<C-s-down>") #'ds/exwm-window-grow-below)
        (exwm-input-set-key (kbd "<C-M-s-down>") #'ds/exwm-window-shrink-below)

        (exwm-input-set-key (kbd "<C-s-left>") #'ds/exwm-window-grow-left)
        (exwm-input-set-key (kbd "<C-M-s-left>") #'ds/exwm-window-shrink-left)

        (exwm-input-set-key (kbd "<C-s-right>") #'ds/exwm-window-grow-right)
        (exwm-input-set-key (kbd "<C-M-s-right>") #'ds/exwm-window-shrink-right)

        ;;resize to ratio
        (exwm-input-set-key (kbd "s-=") #'ds/set-window-ratio)

        (defun ds/exwm-to-16:9 ()
          (interactive)
          (ds/set-window-ratio nil 16 9 t))

        (exwm-input-set-key (kbd "C-s-=") #'ds/exwm-to-16:9)
        
        (defun ds/exwm-list-x-windows ()
          "Get list if all EXWM managed X windows."
          (let ((names ()))
            (dolist (pair exwm--id-buffer-alist)
              (with-current-buffer (cdr pair)
                ;; (setq names (append names `(,(replace-regexp-in-string "^ " "" (buffer-name)))))))
                (setq names (append names `(,(buffer-name))))))
            names))

        (defun ds/exwm-switch-to-x-window (buffer-or-name)
          "Switch to EXWM managed X window BUFFER-OR-NAME."
          (interactive (list (completing-read "Select Window: " (ds/exwm-list-x-windows) nil t)))
          (exwm-workspace-switch-to-buffer buffer-or-name))

        (defun ds/exwm-bring-window-here (buffer-or-name)
          "Move an EXWM managed X window BUFFER-OR-NAME to the current workspace."
          (interactive (list (completing-read "Bring Window: " (ds/exwm-list-x-windows) nil t)))
          (with-current-buffer buffer-or-name
            (exwm-workspace-move-window exwm-workspace--current exwm--id)
            (switch-to-buffer (exwm--id->buffer exwm--id))))

        (exwm-input-set-key (kbd "s-d") #'ds/exwm-switch-to-x-window)

        (exwm-input-set-key (kbd "C-s-d") #'ds/exwm-bring-window-here)

        ;; alias the C-x o binding to s-o
        (exwm-input-set-key (kbd "s-o") #'other-window)
        (defun ds/exwm-quit ()
          "Close a window in EXWM.

     If it is an X window, then kill the buffer.
     If it is not an X window, delete the window unless it is the only one."
          (interactive)
          (if (equal major-mode 'exwm-mode)
              (kill-buffer))
          (if (> (length (window-list)) 1)
              (delete-window)))
        (exwm-input-set-key (kbd "C-s-q") #'ds/exwm-quit)
        
        ;; popup eshell
        (ds/popup-thing ds/exwm-popup-shell "*Popup Shell*"
                        (let ((eshell-buffer-name "*Popup Shell*"))
                          (eshell t)))
        (exwm-input-set-key (kbd "s-m") #'ds/exwm-popup-shell)

        ;; rules for displaying the popup buffer
        (ds/popup-thing-display-settings "*Popup Shell*" top -1 0.4)

        ;; 's-return': Launch new eshell
        (exwm-input-set-key (kbd "<s-return>")
                            (lambda ()
                              (interactive)
                              (eshell t)))

        ;; 'C-s-return': Launch new Termite window
        (exwm-input-set-key (kbd "<C-s-return>")
                            (lambda ()
                              (interactive)
                              (start-process-shell-command "termite" nil "termite")))
        (ds/popup-thing ds/exwm-popup-telegram "TelegramDesktop"
                        (start-process-shell-command "telegram" nil "telegram-desktop"))

        (ds/popup-thing-display-settings "TelegramDesktop" left -1 100)

        (exwm-input-set-key (kbd "<s-f5>") #'ds/exwm-popup-telegram)
        (ds/popup-thing ds/exwm-popup-pavucontrol "Pavucontrol"
                        (start-process-shell-command "pavucontrol" nil "pavucontrol"))

        (ds/popup-thing-display-settings "Pavucontrol" bottom 0 30)

        (exwm-input-set-key (kbd "<s-f7>") #'ds/exwm-popup-pavucontrol)
        (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                            (lambda ()
                              (interactive)
                              (start-process "volume-up" nil (executable-find "pulseaudio-ctl") "up")))

        (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                            (lambda ()
                              (interactive)
                              (start-process "volume-down" nil (executable-find "pulseaudio-ctl") "down")))

        (exwm-input-set-key (kbd "<XF86AudioMute>")
                            (lambda ()
                              (interactive)
                              (start-process "volume-mute" nil (executable-find "pulseaudio-ctl") "mute")))
        (setq exwm-input-simulation-keys
              '(
                ;; movement
                ([?\C-b] . left)
                ([?\M-b] . C-left)
                ([?\C-f] . right)
                ([?\M-f] . C-right)
                ([?\C-p] . up)
                ([?\C-n] . down)
                ([?\C-a] . home)
                ([?\C-e] . end)
                ([?\M-v] . prior)
                ([?\C-v] . next)
                ([?\C-d] . delete)
                ([?\C-k] . (S-end ?\C-x))
                ;; cut/paste.
                ([?\C-w] . ?\C-x)
                ([?\M-w] . ?\C-c)
                ([?\C-y] . ?\C-v)
                ;; undo/redo
                ([?\C-/] . ?\C-z)
                ([?\C-?] . ?\C-\S-z)
                ;; search
                ([?\C-s] . ?\C-f)))
        (defun ds/exwm-keyrules-termite ()
          (if (and exwm-class-name
                   (string= exwm-class-name "Termite"))
              (exwm-input-set-local-simulation-keys
               '(
                 ([?\C-c ?\C-c] . [?\C-c])
                 ([?\C-b] . left)
                 ([?\M-b] . [?\M-b])
                 ([?\C-f] . right)
                 ([?\M-f] . [?\M-f])
                 ([?\C-p] . up)
                 ([?\C-n] . down)
                 ([?\C-a] . [?\C-a])
                 ([?\C-e] . [?\C-e])
                 ([?\C-d] . [?\C-d])
                 ([?\C-w] . [?\C-\S-x])
                 ([?\M-w] . [?\C-\S-c])
                 ([?\C-y] . [?\C-\S-v])))))
        
        (defun ds/exwm-keyrules-firefox ()
          (if (and exwm-class-name
                   (string-match-p "[Ff]irefox" exwm-class-name))
              (exwm-input-set-local-simulation-keys
               '(
                 ;; movement
                 ([?\C-b] . left)
                 ([?\M-b] . C-left)
                 ([?\C-f] . right)
                 ([?\M-f] . C-right)
                 ([?\C-p] . up)
                 ([?\C-n] . down)
                 ([?\C-a] . home)
                 ([?\C-e] . end)
                 ([?\M-v] . prior)
                 ([?\C-v] . next)
                 ([?\C-d] . delete)
                 ([?\C-k] . (S-end ?\C-x))
                 ;; close tab
                 ([?\M-k] . ?\C-w)
                 ;; cut/paste.
                 ([?\C-w] . ?\C-x)
                 ([?\M-w] . ?\C-c)
                 ([?\C-y] . ?\C-v)
                 ;; undo/redo
                 ([?\C-/] . ?\C-z)
                 ([?\C-?] . ?\C-\S-z)
                 ;; search
                 ([?\C-s] . ?\C-f)))))

        (add-hook 'exwm-manage-finish-hook #'ds/exwm-keyrules-termite)
        (add-hook 'exwm-manage-finish-hook #'ds/exwm-keyrules-firefox)
        (exwm-input-set-key (kbd "s-p") #'password-store-copy)
        (exwm-input-set-key (kbd "C-s-p") #'ds/password-store-get-otp)
        (exwm-input-set-key (kbd "C-M-S-s-l") #'ds/lock-screen)
        (define-key global-map (kbd "C-x C-z") #'ds/lock-screen)
        (define-key global-map (kbd "C-z") #'ds/lock-screen)
        
        ;; notifications
        (ds/popup-thing ds/exwm-eosd "*notifications*"
                        (eosd))
        (ds/popup-thing-display-settings "*notifications*" right 1)

        (exwm-input-set-key (kbd "s-n") #'ds/exwm-eosd)

        ;; auto refresh and auto popup notifications
        (add-function :after (symbol-function 'ds/exwm-eosd) #'ds/exwm-refresh-notification-buffer)

        ;; enable pinentry
        (setq pinentry-popup-prompt-window nil)
        ;; start the server so `emacsclient' can be used as the EDITOR
        (server-start)
        ;; start exwm
        (exwm-enable))
      
      (use-package exwm-randr
        :demand t
        :after exwm
        :init
        (defun ds/display-connected-p (name)
          "Test if display NAME is connected."
          (let* ((test-string (format "%s connected" name))
                 (shell-cmd (format "xrandr | grep -o '^%s' | tr -d '\n'" test-string)))
            (equal test-string (shell-command-to-string shell-cmd))))

        (defun ds/list-displays ()
          "List all displays this machine can handle."
          (split-string
           (shell-command-to-string
            "xrandr | grep -Eo '^[A-Za-z0-9-]+ (dis)?connected' | awk '{print $1}' | tr '\n' ' '")))

        (defun ds/laptop-display-name ()
          "Get laptop internal display name ."
          (shell-command-to-string
           "xrandr | grep -Eo '^eDP[A-Za-z0-9-]+ connected' | awk '{print $1}' | tr -d '\n'"))

        (defun ds/laptop-external-display-name ()
          "Get laptop external display name ."
          (shell-command-to-string
           "xrandr | grep -Eo '^[^e][A-Za-z0-9-]+ connected' | awk '{print $1}' | tr -d '\n'"))

        (defun ds/restart-bar ()
          "Restart whatever bar is being used."
          (interactive)
          (start-process-shell-command
           "startpanel" "*yabar*" (expand-file-name (concat user-emacs-directory "exwm/bin/start-bar"))))

        (defun ds/xrandr-other-displays-off (target)
          "Get a string to run off all displays except for the TARGET."
          (mapconcat
           (lambda (d)
             (concat "--output " d " --off"))
           (seq-filter
            (lambda (d)
              (not (string= d target)))
            (ds/list-displays))
           " "))

        (defun ds/connect-laptop-external ()
          "Connect the laptop to it's external display, no display on laptop screen"
          (interactive)
          (start-process-shell-command
           "xrandr" nil (concat "xrandr --output "
                                (ds/laptop-external-display-name)
                                " --primary --auto "
                                (ds/xrandr-other-displays-off (ds/laptop-external-display-name))))
          (ds/restart-bar))

        (defun ds/disconnect-laptop-external ()
          "Connect laptop display, no external display"
          (interactive)
          (start-process-shell-command
           "xrandr" nil (concat "xrandr --output "
                                (ds/laptop-display-name)
                                " --primary --auto "
                                (ds/xrandr-other-displays-off (ds/laptop-display-name))))
          (ds/restart-bar))

        (defun ds/exwm-auto-screens ()
          "Detect known display setups and set screens accordingly."
          (interactive)
          (let ((laptop-display (ds/display-connected-p (ds/laptop-display-name)))
                (laptop-display-external (ds/display-connected-p (ds/laptop-external-display-name))))
            ;; check for laptop external display
            (if laptop-display
                (if laptop-display-external
                    (ds/connect-laptop-external)
                  (ds/disconnect-laptop-external)))
            (start-process "" nil (concat user-emacs-directory "exwm/bin/wallpaper"))))

        :config
        ;; (add-hook 'exwm-randr-screen-change-hook #'ds/powerline-set-height)
        (add-hook 'exwm-randr-screen-change-hook #'ds/exwm-auto-screens)
        (exwm-randr-enable)
        (ds/restart-bar))

      (use-package exwm-systemtray
        :demand t
        :config
        (exwm-systemtray-enable))))

(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
