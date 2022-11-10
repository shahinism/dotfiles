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
;; (use-package eglot
;;   :init
;;   (customize-set-variable 'eglot-autoshutdown t)

;;   ;; Disable flymake, I tend to use flycheck instead
;;   (customize-set-variable 'eglot-stay-out-of '(flymake))
;; )

;;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  )

(use-package devdocs)

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
(use-package consult-eglot
  :after consult eglot)

;; Fix trailing spaces but only in modified lines
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package yaml-mode)

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(use-package rainbow-mode)

(provide 'lt-programming)
