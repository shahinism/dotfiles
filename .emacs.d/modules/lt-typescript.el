;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'typescript-mode)
(lt/install-package 'js2-refactor)

;; Functions
(defun typescript-doc ()
  (interactive)
  (setq-local dash-docs-docsets '("TypeScript")))

;; Hooks
(when (fboundp #'eglot-ensure)
  (add-hook 'typescript-mode-hook #'eglot-ensure))

(when (fboundp #'dash-docs-search)
  (add-hook 'typescript-mode-hook #'typescript-doc))

(provide 'lt-typescript)
