;;; init.el -*- lexical-binding: t; -*-
(lt/install-package 'hl-todo)
(lt/install-package 'magit)
(lt/install-package 'eglot)
(lt/install-package 'flycheck)
(lt/install-package 'terraform-mode)
(lt/install-package 'yasnippet)
(lt/install-package 'yasnippet-snippets)
(lt/install-package 'dash-docs)
(lt/install-package 'consult-dash)
(lt/install-package 'consult-eglot)
(lt/install-package 'consult-flycheck)
(lt/install-package 'consult-yasnippet)
(lt/install-package 'markdown-mode)
(lt/install-package 'tree-sitter)
(lt/install-package 'tree-sitter-langs)
(lt/install-package 'dumb-jump)
(lt/install-package 'citre)

;; Show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO in the source code
(require 'hl-todo)
(global-hl-todo-mode 1)

;; Citre
(require 'citre)
;; NOTE enabling citre-config, will automatically enable citre-mode
;; where the conditions are met (tag file available), and consequently
;; injects tags as part of autocompletion stack (for both vertico and
;; company) which really sucks!
;; (require 'citre-config)

;; TreeSitter
(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)

;;; Eglot
;;  shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;; Disable flymake, I tend to use flycheck instead
(customize-set-variable 'eglot-stay-out-of '(flymake))

;;; Flycheck
(require 'flycheck)
(global-flycheck-mode)

;; Consult dash
;; https://codeberg.org/ravi/consult-dash
(consult-customize consult-dash :initial (thing-at-point 'symbol))
 
;; Yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Keybindings
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


(provide 'lt-programming)
