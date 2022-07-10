;;; init.el -*- lexical-binding: t; -*-
(lt/install-package 'hl-todo)
(lt/install-package 'magit)
(lt/install-package 'eglot)
(lt/install-package 'flycheck)
(lt/install-package 'terraform-mode)
(lt/install-package 'dash-docs)
(lt/install-package 'consult-dash)
(lt/install-package 'consult-eglot)
(lt/install-package 'consult-flycheck)

;; Show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO in the source code
(require 'hl-todo)
(global-hl-todo-mode 1)

;;; Eglot
;;  shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;;; Flycheck
(require 'flycheck)
(global-flycheck-mode)

;; Consult dash
;; https://codeberg.org/ravi/consult-dash
(consult-customize consult-dash :initial (thing-at-point 'symbol))

(provide 'lt-programming)
