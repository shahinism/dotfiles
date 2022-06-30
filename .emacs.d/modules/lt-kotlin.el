;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'kotlin-mode)

(when (fboundp #'eglot-ensure)
  (add-hook 'kotlin-mode-hook #'eglot-ensure))

(provide 'lt-kotlin)
