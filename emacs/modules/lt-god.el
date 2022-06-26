;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'god-mode)
(require 'god-mode)
(god-mode)

;; Toggle through local mode
;; Future note: It's also possible to toggle all buffers
(global-set-key (kbd "<escape>") #'god-local-mode)

;; Visual indicator
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

;; Extra bindings
(define-key god-local-mode-map (kbd ".") #'repeat)

(provide 'lt-god)
