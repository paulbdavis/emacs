;;; init.el --- dangersalad emacs init               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul B Davis

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

;; Init for Emacs.  

;;; Code:

(use-package ds-basic
  :load-path "site-lisp/")

(use-package ds-theme
  :load-path "site-lisp/"
  :demand t
  :commands (ds/get-zenburn-color ds/setup-zenburn-faces)
  :config
  (ds/setup-zenburn-faces))

(use-package ds-util
  :load-path "site-lisp/"
  :commands (ds/align-repeat)
  :bind (("C-c n" . ds/indent-buffer)
         ("C-o" . ds/open-next-line)
         ("M-o" . ds/open-previous-line)
         ("M-<f12>" . ds/toggle-mode-line)))

(use-package ds-ui
  :load-path "site-lisp/")

(use-package ds-coding
  :load-path "site-lisp/")

(use-package project
  :custom ((project-switch-commands 'project-find-file)
           (project-vc-extra-root-markers '("requirements.txt" "go.mod" "package.json" "Chart.yaml"))))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (dolist (var '("GOPATH" "GOBIN" "PYENV_ROOT" "NVM_DIR"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package csv-mode
  :ensure t)

