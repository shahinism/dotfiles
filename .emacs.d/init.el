;;; init.el -*- lexical-binding: t; -*-

;; Profile Emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

(defmacro lt/install-package (package)
  "Install package if it's not installed"
  `(unless (package-installed-p ,package) (package-install ,package)))

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
(require 'lt-default)
(require 'lt-use-package)
;;(require 'lt-god)
(require 'lt-evil)
(require 'lt-completion)
(require 'lt-editing)
(require 'lt-programming)
(require 'lt-python)
(require 'lt-ui)
(require 'lt-env)
(require 'lt-navigation)
(require 'lt-typescript)
(require 'lt-kotlin)
(require 'lt-org)
;; Automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" default))
 '(package-selected-packages
   '(evil-easymotion which-key org-bullets org-appear general undo-tree evil-nerd-commenter evil-collection evil smartparens kotlin-mode tide js2-refactor typescript-mode terraform-mode ace-window hydra avy direnv esup doom-themes zenburn-theme elisp-demos elisp-mode helpful doom-modeline all-the-icons flycheck pyvenv numpydoc blacken anaconda-mode eglot magit hl-todo vertico orderless marginalia embark-consult embark corfu-doc consult cape god-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
