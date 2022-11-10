;;; init.el -*- lexical-binding: t; -*-

(use-package go-mode
  :after dash
  :config
  ;; Functions
  (defun go-doc ()
    (interactive)
    (setq-local devdocs-current-docs '("go")))

  (when (fboundp #'devdocs-lookup)
    (add-hook 'go-mode-hook #'go-doc))
  )

(provide 'lt-go)
