;;; ds-coding.el --- emacs customizations for coding       -*- lexical-binding: t; -*-

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

;; emacs packages and settings for general coding work

;;; Code:

;; map some shell file names
(add-to-list 'auto-mode-alist '("PKGBUILD$" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("zshenv$" . sh-mode))
(add-to-list 'auto-mode-alist '("zprofile$" . sh-mode))


(use-package emacs
  :custom ((treesit-auto-install-grammar 'always)
           (treesit-enabled-modes t)))

;; setup hooks for some languages
(defun ds/go-ts-mode-setup ()
  (setq go-ts-mode-indent-offset 4))

(defun ds/set-js-lsp-indent ()
  "Setup indent for js/ts/json LSP."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local js-indent-level 2))

(add-hook 'go-ts-mode-hook 'ds/go-ts-mode-setup)
(add-hook 'js-ts-mode-hook 'ds/set-js-lsp-indent)
(add-hook 'typescript-ts-mode-hook 'ds/set-js-lsp-indent)
(add-hook 'json-ts-mode 'ds/set-json-lsp-indent)

(use-package magit
  :ensure t
  :bind (("C-x p v" . magit-status)
         :map magit-mode-map
         ([remap previous-line] . magit-previous-line)
         ([remap next-line] . magit-next-line))
  :defines (magit-merge-arguments
            magit-defines-global-keybinds
            magit-display-buffer-function
            magit-log-highlight-keywords
            magit-diff-highlight-keywords
            magit-repolist-columns)
  :config
  (setq magit-merge-arguments '("--no-ff")
        magit-defines-global-keybinds t
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

(use-package eglot
  :defines (ds/eglot-format-buffer-before-save
            eglot-ensure)
  :init
  (define-prefix-command 'ds/eglot-mode-map)
  (defun ds/setup-eglot-save-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook
              (lambda ()
                (call-interactively 'eglot-code-action-organize-imports))
              nil t))
  :bind (:map eglot-mode-map
              ("C-c l" . ds/eglot-mode-map)
              :map ds/eglot-mode-map
              ("a" . eglot-code-actions)
              ("i" . eglot-find-implementation)
              ("d" . eglot-find-typeDefinition)
              ("q" . eglot-code-action-quickfix)
              ("r" . eglot-rename)
              ("R" . eglot-reconnect))
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode tsx-ts-mode) . ("tsc" "--lsp" "--stdio")))
  (setq-default eglot-workspace-configuration
                '(:typescript
                  (:format (:indentSize 2 :tabSize 2))
                  :javascript
                  (:format (:indentSize 2 :tabSize 2))
                  :gopls
                  ((staticcheck . t)
                   (completeUnimported . t)
                   (templateExtensions . ["tmpl" "html"])
                   (hints . (:assignVariableTypes :json-false
                                                  :compositeLiteralFields :json-false
                                                  :compositeLiteralTypes :json-false
                                                  :constantValues :json-false
                                                  :functionTypeParameters :json-false
                                                  :ignoredError :json-false
                                                  :parameterNames :json-false
                                                  :rangeVariableTypes :json-false)))))
  
  :hook ((go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure)
         ;; ensure save hooks are set up for some
         (go-ts-mode . ds/setup-eglot-save-hooks)
         (typescript-ts-mode . ds/setup-eglot-save-hooks)
         (js-ts-mode . ds/setup-eglot-save-hooks)))

(use-package nvm
  :ensure t
  :commands (nvm-use)
  :custom
  (nvm-dir (or (getenv "NVM_DIR") (concat (getenv "HOME") "/.local/share/nvm")))
  :config
  (nvm-use "24"))

(use-package pyvenv
  :ensure t
  :commands (pyvenv-mode)
  :config
  (pyvenv-mode))

(use-package pyenv
  :ensure t
  :vc (:url "https://github.com/aiguofer/pyenv.el" :branch "master" :rev :newest)
  :config
  (global-pyenv-mode))

(use-package rg
  :ensure t)

(provide 'ds-coding)
;; ds-coding.el ends here
