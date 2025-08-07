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

;; performance stuff (mainly for lsp-mode)
(setq gc-cons-threshold 12800000)
;; 4mb
(setq read-process-output-max (* 4 1024 1024))

(setq load-path (cons (concat user-emacs-directory "lib") load-path))
(setq load-path (cons (concat user-emacs-directory "packages") load-path))

(require 'package-loader)

(use-package ds-basic)
(use-package ds-theme
  :commands (ds/get-zenburn-color))

(use-package tts-editor
  :straight (tts-editor :type git
                        :host github
                        :repo "dangersalad/emacs-tts-editor")
  :commands (tts-editor/listen-start
             tts-editor/listen-stop))

(use-package ds-util
  :commands (ds/find-eslint-executable ds/get-zenburn-color)
  :bind (("C-c _" . ds/toggle-camelcase-underscores)
         ("C-o" . ds/open-next-line)
         ("C-c n" . ds/indent-buffer)
         ("M-o" . ds/open-previous-line)))

(use-package vterm
  :straight t
  :demand
  :commands (vterm ds/vterm)
  :custom ((vterm-max-scrollback 10000))
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

(use-package multi-vterm
  :straight t
  :demand
  :init
  (defvar ds/multi-vterm-map (make-sparse-keymap)
    "Keymap for multi-vterm commands.")
  (defun ds/multi-vterm-create (name)
    "Create a vterm buffer and set it's name to NAME."
    (interactive "sName: ")
    (let* ((bufname (if (< 0 (length name)) (concat "*vterminal<" name ">*")))
           (existing-buf (get-buffer bufname)))
      (if (buffer-live-p existing-buf)
          (switch-to-buffer existing-buf)
        (progn (multi-vterm)
               (if bufname (rename-buffer bufname))))))
  (defun ds/multi-vterm-dedicated-solo ()
    "Open the multi-vterm-dedicated buffer and make it the only window in the frame."
    (interactive)
    (multi-vterm-dedicated-close)
    (multi-vterm-dedicated-open)
    (delete-other-windows))
  :commands (multi-vterm
             multi-vterm-next
             multi-vterm-prev
             multi-vterm-dedicated-toggle
             multi-vterm-dedicated-open
             multi-vterm-dedicated-close
             multi-vterm-project
             ds/multi-vterm-create
             ds/multi-vterm-dedicated-solo)
  :bind (
         :map project-prefix-map
         ("s" . multi-vterm-project)
         :map ds/multi-vterm-map
         ("C-s" . ds/multi-vterm-create)
         ("n" . multi-vterm-next)
         ("p" . multi-vterm-prev))
  :bind-keymap ("C-c C-s" . ds/multi-vterm-map))

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
  :commands (dired-subtree-toggle)
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
  (direnv-mode))

;; git porcelean
(use-package magit
  :straight t
  :bind (("C-x p v" . magit-status)
         :map magit-mode-map
              ([remap previous-line] . magit-previous-line)
              ([remap next-line] . magit-next-line))
  :defines (magit-merge-arguments
            magit-defines-global-keybinds)
  :config
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-defines-global-keybinds t
        magit-display-buffer-function 'display-buffer
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
          ("Path" 99 magit-repolist-column-path nil))))

(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))

;; compilation settings
(use-package compile
  :config
  (define-key compilation-mode-map (kbd "q") #'delete-frame)
  (setq compilation-finish-functions nil)
  (setq compilation-scroll-output t))

;; highlight parens in emacs lisp mode
(use-package highlight-parentheses
  :straight t
  :diminish highlight-parentheses-mode
  :hook (emacs-lisp-mode . highlight-parentheses-mode)
  :defines (highlight-parentheses-background-colors
            highlight-parentheses-colors)
  :config
  (setq highlight-parentheses-background-colors
        `(,(ds/get-zenburn-color "bg-2")
          ,(ds/get-zenburn-color "bg-1")
          ,(ds/get-zenburn-color "bg-05")
          ,(ds/get-zenburn-color "bg+05")
          ,(ds/get-zenburn-color "bg+1")
          ,(ds/get-zenburn-color "bg+2")
          ,(ds/get-zenburn-color "bg+3")
          ,(ds/get-zenburn-color "fg-1")))
  (setq highlight-parentheses-colors
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

;; project management
(use-package project
  :custom ((project-switch-commands 'project-find-file)))

;; search
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; project.el rebinds
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x p g" . consult-ripgrep)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)
   consult-buffer :preview-key '(:debounce 1 any)
   consult-man :preview-key nil
   consult-buffer-other-window :preview-key '(:debounce 0.4 "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; minibuffer completion stuff
;; Enable vertico
(use-package vertico
  :straight t
  :custom ((vertico-resize t))
  :bind (:map vertico-map
              ("C-e" . vertico-insert))
  :init
  (vertico-mode))

;; avy for jumping around
(use-package avy
  :straight t
  :custom ((avy-keys '(?t ?n ?h ?e ?s ?o ?a ?i ?g ?y)))
  :bind (:map goto-map
              ("j" . avy-goto-word-0)
              ("M-j" . avy-goto-word-1)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :straight t
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



(use-package corfu
  :straight t
  :custom ((corfu-auto t))
  :init
  (global-corfu-mode)
  :config
  (setq corfu-quit-no-match 'separator))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package treesit-auto
  :straight t
  :custom ((treesit-auto-install 'prompt))
  :init
  (defun ds/go-ts-mode-setup ()
    (setq go-ts-mode-indent-offset 4))
  (defun ds/set-js-lsp-indent ()
    "Setup indent for javascipt LSP."
    (setq indent-tabs-mode nil))
  :hook ((go-ts-mode . ds/go-ts-mode-setup)
         (js-ts-mode . ds/set-js-lsp-indent))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; LSP stuff
(use-package yasnippet
  :straight t
  :commands yas-minor-mode
  :config
  (set-face-attribute 'yas-field-highlight-face nil :inherit nil)
  :hook (go-ts-mode . yas-minor-mode))

(use-package all-the-icons
  :straight t)

(use-package nerd-fonts
  :straight (nerd-fonts :type git
                        :host github
                        :repo "twlz0ne/nerd-fonts.el")
  :config
  (defun ds/nerd-font-icon (name &rest args)
    (let ((icon (nerd-fonts name))
          (other-face (plist-get args :face))
          (height (or (plist-get args :height) 1.0))
          (v-adjust (or (plist-get args :v-adjust) 0.0)))
      (unless icon
        (error (format "Invalid nerd font icon `%s'" name)))
      (let ((face (if other-face
                      `(:height ,height :inherit ,other-face)
                    `(:height ,height))))
        (propertize icon
                    'face face
                    'font-lock-face face
                    'display `(raise ,v-adjust)
                    'rear-nonsticky t)))))

(use-package eglot
  :defines (ds/eglot-format-buffer-before-save
            eglot-ensure)
  :init
  (defun ds/setup-eglot-save-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook
              (lambda ()
                (call-interactively 'eglot-code-action-organize-imports))
              nil t))
  :config
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (completeUnimported . t)
                           (templateExtensions . ["tmpl" "html"])
                           (hints . (:assignVariableTypes t
                                                          :compositeLiteralFields t
                                                          :compositeLiteralTypes t
                                                          :constantValues t
                                                          :functionTypeParameters t
                                                          :ignoredError t
                                                          :parameterNames t
                                                          :rangeVariableTypes t))))))
  :hook ((go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure)
         ; ensure save hooks are set up for some
         (go-ts-mode . ds/setup-eglot-save-hooks)
         (typescript-ts-mode . ds/setup-eglot-save-hooks)
         (js-ts-mode . ds/setup-eglot-save-hooks))
  )

;; frames only
(use-package frames-only-mode
  :straight t
  :custom ((frames-only-mode-kill-frame-when-buffer-killed-buffer-list
            ;; '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*" "*HTTP Response*" "*HTTP Headers*")))
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

(add-to-list 'Info-directory-list (concat (getenv "HOME") "/.local/share/info"))

(defun ds/toggle-mode-line ()
  "Toggle the modeline on and off."
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)) )
  (redraw-display))

(global-set-key [M-f12] #'ds/toggle-mode-line)

;; some basic modes for files I work on
(use-package csv-mode
  :straight t)
(use-package nginx-mode
  :straight t)
(use-package json-mode
  :straight t)
(use-package dockerfile-mode
  :straight t)
(use-package markdown-mode
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


;; html/web
(use-package web-mode
  :straight t
  :mode ("\\.html\\'")
  :custom ((web-mode-code-indent-offset 2 "Set indent for code")
           (web-mode-markup-indent-offset 2 "Set indent for markup")
           (web-mode-enable-auto-indentation nil "Disable auto indent")
           (web-mode-comment-formats '(("java" . "/*")
                                       ("javascript" . "//")
                                       ("typescript" . "//")
                                       ("php" . "/*")
                                       ("css" . "/*")))
           (web-mode-part-padding 0 "Set padding to 0")
           (web-mode-script-padding 0 "Set padding to 0")
           (web-mode-style-padding 0 "Set padding to 0")
           (web-mode-comment-style 2 "Set non-annowying comment style")))

(use-package lua-mode
  :straight t)

(use-package scad-mode
  :straight t)

(use-package nvm
  :straight t
  :commands (nvm-use)
  :config
  (nvm-use "20"))

(use-package zig-mode
  :straight t)

(use-package pico8-mode
  :straight (pico8-mode :type git
                        :host github
                        :repo "Kaali/pico8-mode")
  :init
  (defun ds/setup-pico8-mode ()
    "Setup pico8-mode"
    (setq-local lua-indent-level 1
                indent-tabs-mode 'only
                tab-width 1))
  :hook ((pico8-mode . ds/setup-pico8-mode)))


(use-package structurizr-mode
  :straight (strucurizr-mode :type git
                             :host github
                             :repo "gilesp/structurizr-mode"))
;; searching
(use-package rg :straight t)

(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here

