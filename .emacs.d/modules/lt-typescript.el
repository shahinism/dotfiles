;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'typescript-mode)
(lt/install-package 'js2-refactor)
(lt/install-package 'tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'lt-typescript)
