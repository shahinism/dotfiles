;;; init.el -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :config

  (when (fboundp #'eglot-ensure)
    (add-hook 'kotlin-mode-hook #'eglot-ensure))
)
(provide 'lt-kotlin)
