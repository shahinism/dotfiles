;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'typescript-mode)
(lt/install-package 'js2-refactor)

(when (fboundp 'eglot-ensure)
  (progn
    (add-hook 'typescript-mode-hook #'eglot-ensure)))

(provide 'lt-typescript)
