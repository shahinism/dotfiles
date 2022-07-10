;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'exec-path-from-shell)

(require 'exec-path-from-shell)

(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(provide 'lt-linux)
