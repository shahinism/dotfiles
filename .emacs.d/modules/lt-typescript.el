;;; init.el -*- lexical-binding: t; -*-

(use-package js2-refactor)

;; Functions
(defun typescript-doc ()
  (interactive)
  (setq-local devdocs-current-docs '("typescript")))

(use-package typescript-mode
  :config

  ;; Hooks
  (when (fboundp #'devdocs-lookup)
    (add-hook 'typescript-mode-hook #'typescript-doc))
  )

(provide 'lt-typescript)
