;;; ds-eshell.el --- eshell setup                    -*- lexical-binding: t; -*-

;; Copyright (C) 2018 paul davis

;; Author:  <paul@sputnik>
;; Keywords: eshell

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

(use-package dash
  :functions -reduce-from
  :ensure t)

(use-package s
  :functions s-blank
  :ensure t)

(use-package eshell
  :hook ((eshell-mode . ds/eshell-setup)
         (eshell-pre-command . ds/eshell-append-history)
         (eshell-post-command . eshell-read-history))
  :defines (eshell-mode-map
            eshell-history-ring
            eshell-history-file-name
            eshell-prompt-regexp
            eshell-password-prompt-regexp
            eshell-scroll-to-bottom-on-input
            eshell-error-if-no-glob
            eshell-hist-ignoredups
            eshell-save-history-on-exit
            eshell-prefer-lisp-functions
            eshell-history-size
            eshell-destroy-buffer-when-process-dies
            eshell-prompt-function
            eshell-visual-commands)
  :functions (eshell-write-history eshell/pwd)
  :config
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient")

  ;; add "pin" to the list of words for detecting password entry from eshell
  (push "pin" password-word-equivalents)
  
  (setq eshell-prompt-regexp "^ [$#] "
        eshell-password-prompt-regexp (format "\\(%s\\).*:\\s *\\'" (regexp-opt password-word-equivalents))
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit nil
        eshell-prefer-lisp-functions nil
        eshell-history-size 4096
        eshell-destroy-buffer-when-process-dies t
        ;; Enable the new eshell prompt
        eshell-prompt-function 'ds/eshell-prompt-func)

  (defun ds/eshell-setup ()
    (setenv "PAGER" "cat")
    (setenv "EDITOR" "emacsclient")
    (use-package eshell
      :bind (:map
             eshell-mode-map ("C-l" . eshell/clear)))
    (direnv-update-environment)
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail")
    (add-to-list 'eshell-visual-commands "top")
    (add-to-list 'eshell-visual-commands "htop")
    (setq eshell-path-env (getenv "PATH"))
    (set-face-attribute 'eshell-prompt-face nil
                        :foreground (ds/get-zenburn-color "fg")
                        :weight 'normal))


  (defun ds/eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    
    (when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t))))))

(defvar company-require-match nil)
(make-variable-buffer-local 'company-require-match)
(use-package esh-autosuggest
  :ensure t
  :hook ((eshell-mode . esh-autosuggest-mode)
         (eshell-mode . ds/esh-autosuggest-setup))
  :init
  (require 'subr-x)
  (defun ds/esh-autosuggest-setup ()
    (set (make-local-variable 'company-minimum-prefix-length) 1)
    (set (make-local-variable 'company-require-match) nil)
    (face-remap-add-relative 'company-preview-common 'ds/esh-autosuggest-face)))


(use-package pcmpl-args
  :ensure t
  :config

  (use-package pcmpl-git
    :ensure t)

  ;; ============================================================
  ;;
  ;; pacman completion
  ;;
  ;; ============================================================
  (defvar pcomplete-pacman-installed-packages
    (split-string (shell-command-to-string "pacman -Qq"))
    "p-completion candidates for `pacman' regarding installed packages")

  (defvar pcomplete-pacman-web-packages
    (split-string (shell-command-to-string "pacman -Slq"))
    "p-completion candidates for `pacman' regarding packages on the web")

  (defun pcomplete/pacman ()
    "Completion rule for the `pacman' command."
    (pcomplete-opt "DFQRSUilos")
    (cond ((pcomplete-test "-[DRQ][a-z]*")
           (pcomplete-here pcomplete-pacman-installed-packages))
          ((pcomplete-test "-[FS][a-z]*")
           (pcomplete-here pcomplete-pacman-web-packages))
          (t (pcomplete-here (pcomplete-entries)))))

  ;; ============================================================
  ;;
  ;; systemctl completion
  ;;
  ;; ============================================================
  (defvar pcomplete-systemctl-commands
    '("disable" "enable" "status" "start" "restart" "stop" "daemon-reload")
    "p-completion candidates for `systemctl' main commands")

  (defvar pcomplete-systemd-units
    (split-string
     (shell-command-to-string
      "(systemctl list-units --all --full --no-legend;systemctl list-unit-files --full --no-legend)|while read -r a b; do echo \" $a\";done;"))
    "p-completion candidates for all `systemd' units")

  (defvar pcomplete-systemd-user-units
    (split-string
     (shell-command-to-string
      "(systemctl list-units --user --all --full --no-legend;systemctl list-unit-files --user --full --no-legend)|while read -r a b;do echo \" $a\";done;"))
    "p-completion candidates for all `systemd' user units")

  (defun pcomplete/systemctl ()
    "Completion rules for the `systemctl' command."
    (pcomplete-here (append pcomplete-systemctl-commands '("--user")))
    (cond ((pcomplete-test "--user")
           (pcomplete-here pcomplete-systemctl-commands)
           (pcomplete-here pcomplete-systemd-user-units))
          ((pcomplete-test "daemon-reload")
           (pcomplete-here))
          (t (pcomplete-here pcomplete-systemd-units))))

  ;; ============================================================
  ;;
  ;; kubectl completion
  ;;
  ;; ============================================================
  (defvar pcomplete-kubectl-commands
    '("get" "delete" "log" "rollout" "describe"))
  
  (defvar pcomplete-kubectl-resources
    '("deployment" "pod" "hpa" "ingress" "service" "cert" "job" "serviceaccount" "clusterrole" "role" "clusterrolebinding" "rolebinding"))
  
  (defvar pcomplete-kubectl-global-opts
    '("-n" "--namespace" "--all-namespaces"))

  (defvar pcomplete-kubectl-log-opts
    '("-f" "-p" "--tail="))

  (defun ds/kubectl-get-namespaces ()
    "Get k8s namespaces."
    (split-string (shell-command-to-string "kubectl get namespace -o go-template='{{range .items}}{{ .metadata.name }} {{ end }}'")))
  
  (defun ds/kubectl-get-pods (namespace)
    "Get k8s pods in NAMESPACE."
    (let ((ns (if (string= "--all-namespaces" namespace)
                  namespace
                (if namespace
                    (format "-n %s" namespace)
                  ""))))
      (split-string (shell-command-to-string (format "kubectl %s get pod -o go-template='{{range .items}}{{ .metadata.name }} {{ end }}'" ns))))
    )

  (defun ds/kubectl-get-namespace-arg (args)
    "Gets the namespace argument from ARGS."
    (if (eq args '()) nil
      (let ((arg (car args))
            (rest (cdr args)))
        (cond
         ((or (string= arg "--namespace") (string= arg "-n"))
          (car rest))
         ((string= arg "--all-namespaces")
          arg)
         (t (ds/kubectl-get-namespace-arg rest))))))
  
  (defun pcomplete/kubectl ()
    "Completion rules for the `kubectlctl' command."
    (pcomplete-here (append pcomplete-kubectl-commands pcomplete-kubectl-global-opts))
    (cond
     ((pcomplete-match (regexp-opt '("-n" "--namespace")) 1)
      (pcomplete-here (ds/kubectl-get-namespaces))
      (pcomplete-here pcomplete-kubectl-commands))
     (t (pcomplete-here pcomplete-kubectl-commands)))

    (cond
     ((pcomplete-match "log" 1)
      (pcomplete-here (append pcomplete-kubectl-log-opts (ds/kubectl-get-pods (ds/kubectl-get-namespace-arg pcomplete-args)))))
     (t (pcomplete-here pcomplete-kubectl-resources)))
    
    (cond
     ((pcomplete-match "pod" 1)
      (pcomplete-here (ds/kubectl-get-pods (ds/kubectl-get-namespace-arg pcomplete-args))))
     ((pcomplete-match "log" 2)
      (pcomplete-here (append pcomplete-kubectl-log-opts (ds/kubectl-get-pods (ds/kubectl-get-namespace-arg pcomplete-args)))))
     ((pcomplete-match "log" 3)
      (pcomplete-here (append pcomplete-kubectl-log-opts (ds/kubectl-get-pods (ds/kubectl-get-namespace-arg pcomplete-args)))))
     ((pcomplete-match "log" 4)
      (pcomplete-here (ds/kubectl-get-pods (ds/kubectl-get-namespace-arg pcomplete-args)))))
    )

  )


(use-package eshell
  :init
  (require 'dash)
  (require 's)
  (require 'magit)
  (defvar ds/eshell-sep " | "
    "Separator between esh-sections")

  (defvar ds/eshell-section-delim " "
    "Separator between an esh-section icon and form")

  (defvar ds/eshell-header "\n "
    "Eshell prompt header")

  (defmacro ds/with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro ds/eshell-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(defvar ,NAME
       (lambda () (when ,FORM
                    (let ((result (concat ,ICON (if (> (length ,ICON) 0) ds/eshell-section-delim "") ,FORM)))
                      (if ,@PROPS
                          (ds/with-face result ,@PROPS)
                        result))))
       "Eshell prompt section - ,NAME"))


  (defun ds/split-directory-prompt (directory)
    (if (string-match-p ".*/.*" directory)
        (list (file-name-directory directory) (file-name-base directory))
      (list "" directory)))

  (defun ds/pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let ((p-lst (split-string pwd "/")))
      (if (> (length p-lst) 2)
          (concat
           (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                      (substring elm 0 1)))
                      (butlast p-lst 2)
                      "/")
           "/"
           (mapconcat (lambda (elm) elm)
                      (last p-lst 2)
                      "/"))
        pwd)))  ;; Otherwise, we just return the PWD

  (ds/eshell-section esh-dir
                     (ds/with-face "" `(:foreground ,(ds/get-zenburn-color "fg-1") :weight bold))
                     (let* ((dirparts (ds/split-directory-prompt (ds/pwd-shorten-dirs (abbreviate-file-name (eshell/pwd)))))
                            (parent (car dirparts))
                            (dirname (cadr dirparts)))
                       (concat (ds/with-face parent `(:foreground ,(ds/get-zenburn-color "bg+3")))
                               (ds/with-face dirname `(:foreground ,(ds/get-zenburn-color "fg-1") :weight bold)))))

  (ds/eshell-section esh-git
                     (ds/with-face "" `(:foreground ,(ds/get-zenburn-color "orange")))
                     (let* ((unstaged-count (length (magit-unstaged-files)))
                            (staged-count (length (magit-staged-files)))
                            (untracked-count (length (magit-untracked-files)))
                            (unstaged (if (> unstaged-count 0)
                                          (ds/with-face
                                           (concat " (" (number-to-string unstaged-count) ")")
                                           `(:foreground ,(ds/get-zenburn-color "yellow")))
                                        ""))
                            (staged (if (> staged-count 0)
                                        (ds/with-face
                                         (concat " (" (number-to-string staged-count) ")")
                                         `(:foreground ,(ds/get-zenburn-color "green")))
                                      ""))
                            (untracked (if (> untracked-count 0)
                                           (ds/with-face
                                            (concat " (" (number-to-string untracked-count) ")")
                                            `(:foreground ,(ds/get-zenburn-color "red")))
                                         "")))
                       (if (magit-get-current-branch)
                           (concat (ds/with-face (magit-get-current-branch)
                                                 `(:foreground ,(ds/get-zenburn-color "blue")))
                                   staged unstaged untracked)
                         nil)))

  (ds/eshell-section esh-last-command-status
                     ""
                     (if (eq eshell-last-command-status 0)
                         nil
                       (ds/with-face ""  `(:foreground ,(ds/get-zenburn-color "red+1")))))

  (ds/eshell-section esh-clock
                     ""
                     (format-time-string "%H:%M" (current-time)) `(:foreground ,(ds/get-zenburn-color "green")))

  (defun ds/extract-tramp-target (conn-type &optional part)
    (save-match-data
      (let ((dirname (eshell/pwd)))
        (and (string-match (concat conn-type ":\\([^@|:]+\\)@?\\([^@|:]*\\)") dirname)
             (let ((user (match-string 1 dirname))
                   (host (match-string 2 dirname)))
               (cond ((equal part 'user) user)
                     ((equal part 'host) host)
                     ((equal part 'all) (if (not (string= "" host)) (concat user "@" host) user))))))))

  (ds/eshell-section esh-tramp-status
                     ""
                     (let ((ssh-status (ds/extract-tramp-target "ssh" 'all))
                           (sudo-status (ds/extract-tramp-target "sudo" 'user)))
                       (if (and ssh-status sudo-status)
                           (concat (ds/with-face (concat "ssh:" ssh-status) `(:foreground ,(ds/get-zenburn-color "red+1")))
                                   " "
                                   (ds/with-face (concat "sudo:" sudo-status)  `(:foreground ,(ds/get-zenburn-color "yellow"))))
                         (if ssh-status
                             (ds/with-face (concat "ssh:" ssh-status) `(:foreground ,(ds/get-zenburn-color "red+1")))
                           (if sudo-status
                               (ds/with-face (concat "sudo:" sudo-status) `(:foreground ,(ds/get-zenburn-color "yellow"))))))))

  (if (boundp 'set-fontset-font)
      (progn (set-fontset-font t '(#Xf017 . #Xf017) "fontawesome")
             (set-fontset-font t '(#Xf011 . #Xf011) "fontawesome")
             (set-fontset-font t '(#Xf026 . #Xf028) "fontawesome")))

  ;; Choose which eshell-funcs to enable
  (defvar ds/eshell-funcs nil
    "Eshell prompt sections")

  (defun ds/eshell-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (if (and (listp x) (not (functionp x)))
        (concat acc (-reduce-from 'ds/eshell-acc "" x) "\n ")
      (--if-let (funcall x)
          (if (s-blank? acc)
              it
            (concat acc
                    (if (string= "\n" (substring acc (- (length acc) 1) (length acc)))
                        " "
                      ds/eshell-sep)
                    it))
        acc)))


  (defun ds/eshell-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat ds/eshell-header
            (replace-regexp-in-string "\n $" "" (-reduce-from 'ds/eshell-acc "" ds/eshell-funcs))
            "\n"
            (concat " " (if (= (user-uid) 0) "#" "$") " ")))
  ;; Choose which eshell-funcs to enable
  (defvar ds/eshell-funcs nil
    "Eshell prompt sections")

  ;; Choose which eshell-funcs to enable
  (setq ds/eshell-funcs (list (list esh-dir esh-clock) (list esh-git) (list esh-tramp-status esh-last-command-status))))

(provide 'ds-eshell)
;;; ds-eshell.el ends here
