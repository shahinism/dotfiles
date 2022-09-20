;;; init.el -*- lexical-binding: t; -*-

(server-start)

;; Profile Emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

;; Add modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system
(set-default-coding-systems 'utf-8)
(customize-set-variable 'visible-bell 1) ; Visually flash instead of beep!
(customize-set-variable 'large-file-warning-threshold (* 100 1000 1000)) ; Change to around ~100 MB

;; Define the user configuration var and etc folders and ensure they
;; exist:
(defvar lt/config-etc-dir (expand-file-name "etc/" user-emacs-directory)
  "The user's configuration etc/ folder")
(defvar lt/config-var-dir (expand-file-name "var/" user-emacs-directory)
  "The user's configuration var/ folder")

(unless (file-exists-p lt/config-etc-dir)
  (mkdir lt/config-etc-dir))

(unless (file-exists-p lt/config-var-dir)
  (mkdir lt/config-var-dir))

;; Require modules in use
(when (eq system-type 'gnu/linux)
  (require 'lt-linux))

(require 'lt-default)
;; (require 'lt-evil)
(require 'lt-meow)
(require 'lt-completion)
(require 'lt-editing)
(require 'lt-programming)
(require 'lt-python)
(require 'lt-ui)
(require 'lt-env)
(require 'lt-typescript)
(require 'lt-kotlin)
(require 'lt-shell)
(require 'lt-web)
(require 'lt-go)
(require 'lt-note)
(require 'lt-lsp)

;; Automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" default))
 '(package-selected-packages
   '(highlight-indent-guides posframe postframe xclip json-snatcher tree-mode ivy-yasnippet ws-butler all-the-icons-ivy-rich company-box company-prescient all-the-icons-ivy counsel-projectile company-quickhelp company ivy-prescient ivy-rich projectile linum-relative consult-ag citre dirvish editorconfig counsel swiper dumb-jump tree-sitter-langs tree-sitter org-download deft org-roam go-mode crux expand-region yaml-mode web-beautify eshell-syntax-highlighting eshell-did-you-mean esh-help markdown-mode aweshell eshell-z shell-pop eshell-toggle eshell-up kind-icon consult-yasnippet yasnippet-snippets consult-flycheck consult-eglot consult-dash exec-path-from-shell evil-easymotion which-key org-bullets org-appear general undo-tree evil-nerd-commenter evil-collection evil smartparens kotlin-mode tide js2-refactor typescript-mode terraform-mode ace-window hydra avy direnv esup doom-themes zenburn-theme elisp-demos elisp-mode helpful doom-modeline all-the-icons flycheck pyvenv numpydoc blacken anaconda-mode eglot magit hl-todo vertico orderless marginalia embark-consult embark corfu-doc consult cape god-mode use-package))
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-cleanup-buffer-at-process-exit t)
 '(shell-pop-default-directory "~/")
 '(shell-pop-full-span t)
 '(shell-pop-restore-window-configuration t)
 '(shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
