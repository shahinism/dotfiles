;;; init.el -*- lexical-binding: t; -*-

(use-package go-mode
  :after dash
  :config
  ;; Functions
  (defun go-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("Go")))

  ;; Hooks
  (when (fboundp #'eglot-ensure)
    (add-hook 'go-mode-hook #'eglot-ensure))

  (when (fboundp #'dash-docs-search)
    (add-hook 'go-mode-hook #'go-doc))
  )

(provide 'lt-go)
