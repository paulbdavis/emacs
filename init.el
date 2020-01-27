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

(setq load-path (cons (concat user-emacs-directory "lib") load-path))
(setq load-path (cons (concat user-emacs-directory "lib/emacs-org-dnd") load-path))
(setq load-path (cons (concat user-emacs-directory "lib/multi-libvterm") load-path))
(setq load-path (cons (concat user-emacs-directory "packages") load-path))

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
  :straight t
  :demand
  :commands (vterm ds/vterm)
  :init
  (defun ds/vterm-send-C-x ()
    (interactive)
    (vterm-send "C-x"))
  :bind (:map vterm-mode-map
              ("C-c t" . 'vterm-copy-mode)
              ("C-x C-x" . 'ds/vterm-send-C-x)
              :map vterm-copy-mode-map
              ("C-c t" . 'vterm-copy-mode))
  :after ds-theme
  :config
  (defun ds/vterm (&optional name)
    (interactive "MName: ")
    (if (< 0 (length name))
        (if (get-buffer name)
            (switch-to-buffer name)
          (vterm name))
      (vterm))))

(use-package multi-libvterm
  :demand
  :init
  (defvar ds/multi-libvterm-map (make-sparse-keymap)
    "Keymap for multi-libvterm commands.")
  (defun ds/multi-libvterm-create (name)
    "Create a vterm buffer and set it's name to NAME."
    (interactive "sName: ")
    (let ((bufname (if (< 0 (length name)) (concat "*vterminal<" name ">*"))))
      (multi-libvterm bufname)
      (if bufname (rename-buffer bufname))))
  (defun ds/multi-libvterm-dedicated-solo ()
    "Open the multi-libvterm-dedicated buffer and make it the only window in the frame."
    (interactive)
    (multi-libvterm-dedicated-close)
    (multi-libvterm-dedicated-open)
    (delete-other-windows))
  :commands (multi-libvterm
             multi-libvterm-next
             multi-libvterm-prev
             multi-libvterm-dedicated-toggle
             multi-libvterm-dedicated-open
             multi-libvterm-dedicated-close
             multi-libvterm-projectile
             ds/multi-libvterm-create
             ds/multi-libvterm-dedicated-solo)
  :bind-keymap ("C-c C-s" . ds/multi-libvterm-map)
  :bind (:map projectile-command-map
              ("x s" . multi-libvterm-projectile)
              :map ds/multi-libvterm-map
              ("C-s" . ds/multi-libvterm-create)
              ("n" . multi-libvterm-next)
              ("p" . multi-libvterm-prev)))

(use-package lilypond-mode
  :catch (lambda (_ err)
           (message (error-message-string err))))

(use-package window-purpose
  :ensure t
  :disabled
  :config
  (add-to-list 'purpose-user-mode-purposes '(LilyPond-mode . lilypond))
  (purpose-compile-user-configuration)
  (purpose-mode))

;; misc packages for general usability
(use-package adaptive-wrap
  :straight t
  :functions adaptive-wrap-prefix-mode
  :init
  (defvar adaptive-wrap-extra-indent 6)
  (defun ds/wrap-on-visual-line-mode ()
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  :hook (visual-line-mode . ds/wrap-on-visual-line-mode))

(use-package dired-subtree
  :straight t
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
  :straight t
  :bind (("M-j" . mc/mark-next-like-this-symbol)))

(use-package direnv
  :straight t
  :demand
  :config
  (direnv-mode)
  :hook ((eshell-directory-change . direnv-update-directory-environment)))

;; syntax checking
(use-package flycheck
  :straight t
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
  (flycheck-add-mode 'javascript-eslint 'vue-mode)

  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode))

;; ivy/counsel setup
(use-package flx
  :straight t)

(use-package smex
  :straight t
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
  :straight t
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
  :demand
  :straight t)

(use-package ivy-posframe
  :straight t
  :disabled
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          (completion-at-point . ivy-posframe-display-at-point)
          (ivy-completion-in-region . ivy-posframe-display-at-point)
          (t               . nil)))
  (ivy-posframe-mode 1))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history)))

;; project management
(use-package projectile
  :straight t
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
  :straight t
  :after projectile
  :config
  (counsel-projectile-mode))

;; git porcelean
(use-package magit
  :straight t
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
  :straight t)

(use-package pdf-tools
  :straight t
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
  :straight t
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
  :straight t
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
  :straight t)
(use-package json-mode
  :straight t)
(use-package dockerfile-mode
  :straight t)
(use-package markdown-mode
  :straight t)
(use-package systemd
  :straight t)
(use-package yaml-mode
  :straight t
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
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

;; rest client
(use-package restclient
  :straight t
  :config
  (ds/popup-thing-display-settings "*HTTP Response*" left 0 0.25)
  (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

;; golang
(use-package go-mode
  :straight t
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
  :straight t
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

;; protobufs

(setq c-default-style "linux"
      c-basic-offset 4)

(use-package protobuf-mode
  :straight t
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
  :straight t)

(use-package lsp-mode
  :straight t
  :demand
  :after (flycheck)
  :defines (lsp-gopls-hover-kind lsp-gopls-env)
  ;; :disabled
  :custom ((lsp-prefer-flymake nil "Use flycheck instead.")
           (lsp-gopls-hover-kind "FullDocumentation" "Full docs on hover.")
           (lsp-gopls-use-placeholders t "Insert snippets."))
  :commands (lsp lsp-deferred)
  :hook (
         (go-mode . ds/go-mode-lsp)
         (js-mode . ds/js-mode-lsp)
         ;; (vue-mode . ds/js-mode-lsp)
         )
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

    (yas-minor-mode)
    
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
    ;; (setf (flycheck-checker-get 'lsp-ui 'next-checkers) (list 'go-golint-solo))
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  
  (defun ds/js-mode-lsp ()
    "Enable `lsp-mode' in `js-mode' and `vue-mode'."

    (lsp)
    ;; (flycheck-add-next-checker 'lsp-ui 'go-golint 'append)
    (setf (flycheck-checker-get 'lsp-ui 'next-checkers) (list 'javascript-eslint))
    ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
    ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
    )
  

  :config
  (use-package lsp-ui
    :straight t
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
    (set-face-foreground 'lsp-ui-doc-header (ds/get-zenburn-color "fg"))))


(use-package eglot
  :straight t
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
  :straight org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :defines (org-mode-map)
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
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package verb
  :ensure
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-mode-prefix-map))

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

(use-package slime
  :straight t
  :after (highlight-parentheses)
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

;; slime and stumpwm
(use-package stumpwm-mode
  :straight t)

(if (file-exists-p (concat user-emacs-directory "local.el"))
    (load-file (concat user-emacs-directory "local.el")))

(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
