;;; init.el -*- lexical-binding: t; -*-
(lt/install-package 'hl-todo)
(lt/install-package 'magit)
(lt/install-package 'eglot)
(lt/install-package 'flycheck)
(lt/install-package 'terraform-mode)

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

(provide 'lt-programming)
