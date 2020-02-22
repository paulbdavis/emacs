;;; ds-theme.el --- customize and set up theming        -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Paul B Davis

;; Author:  <paul@sputnik>
;; Keywords: faces

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

(defvar ds/zenburn-colors
  '(("zenburn-fg+1"      . "#FFFFEF")
    ("zenburn-fg"        . "#DCDCCC")
    ("zenburn-fg-05"     . "#989888")
    ("zenburn-fg-1"      . "#656555")
    ("zenburn-bg-2"      . "#000000")
    ("zenburn-bg-1"      . "#0C0C0C")
    ("zenburn-bg-05"     . "#121212")
    ("zenburn-bg"        . "#1C1C1C")
    ("zenburn-bg+05"     . "#222222")
    ("zenburn-bg+1"      . "#2C2C2C")
    ("zenburn-bg+2"      . "#3F3F3F")
    ("zenburn-bg+3"      . "#4C4C4C")
    ("zenburn-red+1"     . "#DCA3A3")
    ("zenburn-red"       . "#CC9393")
    ("zenburn-red-1"     . "#BC8383")
    ("zenburn-red-2"     . "#AC7373")
    ("zenburn-red-3"     . "#9C6363")
    ("zenburn-red-4"     . "#8C5353")
    ("zenburn-orange"    . "#DFAF8F")
    ("zenburn-yellow"    . "#F0DFAF")
    ("zenburn-yellow-1"  . "#E0CF9F")
    ("zenburn-yellow-2"  . "#D0BF8F")
    ("zenburn-yellow-4"  . "#B09F6F")
    ("zenburn-green-2"   . "#4F6F4F")
    ("zenburn-green-1"   . "#5F7F5F")
    ("zenburn-green"     . "#7F9F7F")
    ("zenburn-green+1"   . "#8FB28F")
    ("zenburn-green+2"   . "#9FC59F")
    ("zenburn-green+3"   . "#AFD8AF")
    ("zenburn-green+4"   . "#BFEBBF")
    ("zenburn-cyan"      . "#93E0E3")
    ("zenburn-blue+3"    . "#3c3c45")
    ("zenburn-blue+1"    . "#94BFF3")
    ("zenburn-blue"      . "#8CD0D3")
    ("zenburn-blue-1"    . "#7CB8BB")
    ("zenburn-blue-2"    . "#6CA0A3")
    ("zenburn-blue-3"    . "#5C888B")
    ("zenburn-blue-4"    . "#4C7073")
    ("zenburn-blue-5"    . "#366060")
    ("zenburn-magenta-1" . "#BCA3A3")
    ("zenburn-magenta"   . "#C0BED1"))
  "List of Zenburn colors.

Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.

This overrides the colors provided by the `zenburn-theme' package.")

(defun ds/get-zenburn-color (name)
  "Get zenburn color by NAME."
  (let* ((key (concat "zenburn-" name))
         (data (assoc key ds/zenburn-colors)))
    (if data
        (cdr data))))

(defmacro ds/make-zenburn-face (name)
  "Make a face from a color named NAME in `ds/zenburn-colors'."
  (let* ((mapname (concat "zenburn-" (symbol-name name)))
         (facename (make-symbol (concat "ds/" mapname)))
         (color (cdr (assoc mapname ds/zenburn-colors)))
         (docstring (concat "Face for " mapname ".") ))
    `(defface ,facename
       '((((background dark)) :foreground ,color)
         (((background light)) :foreground ,color))
       ,docstring)))

(defun ds/setup-zenburn-faces ()
  "Set up custom faces using zenburn colors."
  (interactive)
  ;; make faces for all the colors

  (dolist (name (list 'red 'orange 'yellow 'green 'cyan 'blue 'magenta))
    (ds/make-zenburn-face name))
  
  ;; default face customizations
  ;; region selection
  (set-face-attribute 'region nil
                      :background (ds/get-zenburn-color "blue+3")
                      :inverse-video t)
  ;; flat mode and header lines
  (set-face-attribute 'header-line nil
                      :background (ds/get-zenburn-color "bg+1")
                      :box nil)
  (set-face-attribute 'mode-line nil
                      :background (ds/get-zenburn-color "bg")
                      :box `(:line-width 4 :color ,(ds/get-zenburn-color "bg"))
                      :overline (ds/get-zenburn-color "bg+1")
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (ds/get-zenburn-color "bg+3")
                      :background (ds/get-zenburn-color "bg")
                      :box `(:line-width 4 :color ,(ds/get-zenburn-color "bg"))
                      :overline (ds/get-zenburn-color "bg+1")
                      :underline nil)
  (set-face-attribute 'fringe nil
                      :background (ds/get-zenburn-color "bg"))
  ;; generic highlight face
  (set-face-attribute 'highlight nil
                      :background (ds/get-zenburn-color "bg+1")
                      :weight 'normal
                      :slant 'normal
                      ;; :box `(:line-width -1 :color ,(ds/get-zenburn-color "orange"))
                      :box nil
                      :underline (ds/get-zenburn-color "orange")
                      :overline nil
                      :foreground nil)
  ;; italic comments
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  ;; eldoc function face
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :foreground (ds/get-zenburn-color "blue-1"))
  ;; set the verticle border color
  (set-face-attribute 'vertical-border nil
                      :foreground (ds/get-zenburn-color "bg+1"))

  ;; auto suggest face for eshell
  (make-face 'ds/esh-autosuggest-face)
  (set-face-attribute 'ds/esh-autosuggest-face nil
                      :foreground (ds/get-zenburn-color "fg-1")
                      :background (ds/get-zenburn-color "bg"))

  (with-eval-after-load 'lsp-mode
    (set-face-attribute 'lsp-face-highlight-read nil
                        :weight 'extra-bold)
    (set-face-attribute 'lsp-face-highlight-write nil
                        :weight 'light))
  
  (with-eval-after-load 'flymake
    (set-face-attribute 'flymake-error nil
                        :underline `(:style wave :color ,(ds/get-zenburn-color "red")))
    (set-face-attribute 'flymake-warning nil
                        :underline `(:style wave :color ,(ds/get-zenburn-color "yellow")))
    (set-face-attribute 'flymake-note nil
                        :underline `(:style wave :color ,(ds/get-zenburn-color "blue"))))

  (with-eval-after-load 'vterm
    (set-face-attribute 'vterm-color-default nil
                        :foreground (ds/get-zenburn-color "fg")
                        :background (ds/get-zenburn-color "bg"))
    (set-face-attribute 'vterm-color-black nil
                        :foreground (ds/get-zenburn-color "bg-2")
                        :background (ds/get-zenburn-color "bg+2"))
    (set-face-attribute 'vterm-color-red nil
                        :foreground (ds/get-zenburn-color "red-1")
                        :background (ds/get-zenburn-color "red"))
    (set-face-attribute 'vterm-color-green nil
                        :foreground (ds/get-zenburn-color "green-1")
                        :background (ds/get-zenburn-color "green"))
    (set-face-attribute 'vterm-color-yellow nil
                        :foreground (ds/get-zenburn-color "yellow-1")
                        :background (ds/get-zenburn-color "yellow"))
    (set-face-attribute 'vterm-color-blue nil
                        :foreground (ds/get-zenburn-color "blue-1")
                        :background (ds/get-zenburn-color "blue"))
    (set-face-attribute 'vterm-color-magenta nil
                        :foreground (ds/get-zenburn-color "magenta-1")
                        :background (ds/get-zenburn-color "magenta"))
    (set-face-attribute 'vterm-color-cyan nil
                        :foreground (ds/get-zenburn-color "orange")
                        :background (ds/get-zenburn-color "orange"))
    (set-face-attribute 'vterm-color-white nil
                        :foreground (ds/get-zenburn-color "fg")
                        :background (ds/get-zenburn-color "fg+1"))
    ))


(use-package zenburn-theme
  :straight t
  :config
  (setq zenburn-override-colors-alist ds/zenburn-colors)
  (load-theme 'zenburn t)
  (ds/setup-zenburn-faces))


(provide 'ds-theme)
;;; ds-theme.el ends here
