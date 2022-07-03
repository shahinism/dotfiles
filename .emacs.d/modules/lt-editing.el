;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'smartparens)
(require 'smartparens-config)
;; (sp-local-pair 'org-mode "*" nil :actions :rem)
(smartparens-global-mode)

(show-paren-mode 1)

(setq show-paren-style 'mixed)
(provide 'lt-editing)
