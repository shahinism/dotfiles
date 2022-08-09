;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'editorconfig)
(lt/install-package 'dirvish)

;; DirVish
(dirvish-override-dired-mode)

(setq dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))

;; parentheses
(electric-pair-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)

(provide 'lt-editing)
