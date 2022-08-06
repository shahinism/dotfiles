;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'go-mode)

;; Functions
(defun go-doc ()
  (interactive)
  (setq-local dash-docs-docsets '("Go")))

;; Hooks
(when (fboundp #'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-ensure))

(when (fboundp #'dash-docs-search)
  (add-hook 'python-mode-hook #'go-doc))


(provide 'lt-go)
