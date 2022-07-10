;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'editorconfig)

;; parentheses
(electric-pair-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)

(provide 'lt-editing)
