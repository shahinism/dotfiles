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
(leaf flycheck
  :doc "On the fly syntax checking."
  :ensure t
  :global-minor-mode global-flycheck-mode
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay . 0)
  )

(leaf devdocs :ensure t)

;; Yasnippet
(leaf yasnippet
  :doc "Template system"
  :url "https://github.com/joaotavora/yasnippet"
  :ensure t
  :hook   (prog-mode-hook . yas-minor-mode)
  :custom (yas-snippet-dirs . '("~/.emacs.d/snippets"))
  :config (yas-reload-all))

(leaf yasnippet-snippets
  :after  yasnippet
  :ensure t)

(leaf consult-yasnippet
  :after  yasnippet consult
  :ensure t)

;; Linum
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(leaf markdown-mode :ensure t)
(leaf dumb-jump :ensure t)
(leaf terraform-mode :ensure t)
(leaf consult-eglot
  :after consult eglot
  :ensure t)

(leaf magit
  :doc "Complete text-based user interface to Git"
  :url "https://magit.vc/"
  :ensure t
  :init
  (setq magit-auto-revert-mode nil))

(leaf rainbow-mode
  :doc "Color letter that indicate the color"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :ensure t
  :hook (prog-mode-hook . rainbow-mode))

(leaf rainbow-delimiters
  :doc "Display brackets in rainbow"
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;; Fix trailing spaces but only in modified lines
(leaf ws-butler
  :hook   (prog-mode . ws-butler-mode)
  :ensure t)

(leaf yaml-mode :ensure t)

(leaf indent-guide
  :hook (prog-mode . indent-guide-mode)
  :ensure t)


(provide 'lt-programming)
