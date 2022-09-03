;;; init.el -*- lexical-binding: t; -*-

;; Show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO in the source code
(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
)
;; Citre
(use-package citre)
;; NOTE enabling citre-config, will automatically enable citre-mode
;; where the conditions are met (tag file available), and consequently
;; injects tags as part of autocompletion stack (for both vertico and
;; company) which really sucks!
;; (require 'citre-config)

;; TreeSitter
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
)

(use-package tree-sitter-langs
  :after tree-sitter)

;;; Eglot
;;  shutdown server when last managed buffer is killed
(use-package eglot
  :init
  (customize-set-variable 'eglot-autoshutdown t)

  ;; Disable flymake, I tend to use flycheck instead
  (customize-set-variable 'eglot-stay-out-of '(flymake))
)

;;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  )

;; Consult dash
;; https://codeberg.org/ravi/consult-dash
(use-package consult-dash
  :after consult dash
  :config
  (consult-customize consult-dash :initial (thing-at-point 'symbol))
  )
 
;; Yasnippet
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
)

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :after yasnippet consult)

;; Linum
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(use-package markdown-mode)
(use-package dumb-jump)
(use-package dumb-jump)
(use-package magit)
(use-package terraform-mode)
(use-package dash-docs)
(use-package consult-eglot
  :after consult eglot)

;; Keybindings
(when (fboundp #'general-evil-define-key)
  (general-evil-define-key 'normal 'global
    :prefix "SPC"
    "pa" 'consult-ag
    "pb" 'project-switch-to-buffer
    "pd" 'project-find-dir
    "pf" 'project-find-file
    "pk" 'project-kill-buffers
    "pr" 'project-query-replace-regexp
    "pc" 'project-compile
    "pp" 'project-switch-project
    "d." 'consult-dash
    "e"  'consult-flycheck
    "is" 'consult-yasnippet
    "ld" 'eglot-find-declaration
    "li" 'eglot-find-implementation
    "lt" 'eglot-find-typeDefinition
    "lr" 'eglot-rename
    "jj" 'dumb-jump-go
    "jo" 'dumb-jump-go-other-window
    "jl" 'dumb-jump-quick-look
    "jb" 'dumb-jump-back
    "cj" 'citre-jump
    "cb" 'citre-jump-back
    "cp" 'citre-peek
    "cu" 'citre-update-this-tags-file)
  )

(provide 'lt-programming)
