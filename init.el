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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://releases.melpa.org/packages/") t)

;; stuff with no remote package dependencies
(use-package ds-basic
  :load-path "site-lisp/")

(use-package ds-util
  :load-path "site-lisp/"
  :commands (ds/align-repeat)
  :bind (("C-c n" . ds/indent-buffer)
         ("C-o" . ds/open-next-line)
         ("M-o" . ds/open-previous-line)
         ("M-<f12>" . ds/toggle-mode-line)))

;; this *only* imports the zenburn theme, though I could probably
;; replace that
(use-package ds-theme
  :load-path "site-lisp/"
  :demand t
  :commands (ds/get-zenburn-color ds/setup-zenburn-faces)
  :config
  (ds/setup-zenburn-faces))

;;; packages that group together (mostly) remote packages

;; ui configuration and enhancement
(use-package ds-ui
  :load-path "site-lisp/")

;; stuff for coding
(use-package ds-coding
  :load-path "site-lisp/")

;; stuff that doesn't really fit into one of the above packages
(use-package vterm
  :ensure t
  :commands (vterm ds/project-vterm)
  :init
  (defun ds/remap-vterm-mode-map ()
    (keymap-set vterm-mode-map "C-c t" #'vterm-copy-mode))
  (defun ds/remap-vterm-copy-mode-map ()
    (keymap-set vterm-copy-mode-map "C-c t" #'vterm-copy-mode))
  (defun ds/project-vterm-name (pname)
    (format "*vterm - %s*" pname))
  (defun ds/project-vterm ()
    "Open a vterm buffer for a project"
    (interactive)
    (if (project-current)
        (let* ((pname (project-name (project-current)))
               (bname (ds/project-vterm-name pname))
               (vbuf (get-buffer bname)))
          (if (buffer-live-p vbuf)
              (display-buffer vbuf)
            (vterm bname)))))

  (defun ds/kill-frame-if-current-buffer-is-vterm ()
    "Kill frames as well when certain buffers are closed.

Only if there is only a single window in the frame, helps stop some
packages spamming frames."
    (when (and (one-window-p)
               (eq major-mode 'vterm-mode))
      (delete-frame)))


  (defun ds/advice-delete-vterm-frame-on-bury (orig-fun &rest args)
    "Delete the frame when burying certain buffers.

Only if there are no other windows in the frame, and if the buffer is in
kill-frame-when-buffer-killed-buffer-list."
    (let ((buf (buffer-name)))
      (apply orig-fun args)
      (with-current-buffer buf
        (when (and (one-window-p)
                   (eq major-mode 'vterm-mode))
          (delete-frame)))))

  :config
  (advice-add #'bury-buffer :around #'ds/advice-delete-vterm-frame-on-bury)
  ;; (advice-remove #'bury-buffer #'ds/advice-delete-vterm-frame-on-bury)

  :bind (:map project-prefix-map
              ("s" . ds/project-vterm))
  
  :hook ((vterm-mode . ds/remap-vterm-mode-map)
         (vterm-copy-mode . ds/remap-vterm-copy-mode-map)
         (kill-buffer . ds/kill-frame-if-current-buffer-is-vterm)))

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

