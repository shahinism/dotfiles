;;; init.el -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :config

  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
)

(provide 'lt-linux)
