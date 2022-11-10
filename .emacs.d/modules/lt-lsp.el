;;; init.el -*- lexical-binding: t; -*-

(leaf posframe :ensure t)

(leaf lsp-bridge
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                        :branch "master" :files ("*"))
  :bind
  ;; (:map lsp-bridge-mode-map
  ;;       ("M-." . lsp-bridge-find-def)
  ;;       ("M-," . lsp-bridge-return-from-def)
  ;;       ("M-?" . lsp-bridge-find-references)
  ;;       ("M-i" . lsp-bridge-lookup-documentation)
  ;;       ("M-n" . lsp-bridge-popup-documentation-scroll-up)
  ;;       ("M-p" . lsp-bridge-popup-documentation-scroll-down)
  ;;       ("s-C-n" . lsp-bridge-jump-to-next-diagnostic)
  ;;       ("s-C-p" . lsp-bridge-jump-to-prev-diagnostic))
  :config
  (require 'lsp-bridge)

  (setq lsp-bridge-auto-format-code-idle 5
        lsp-bridge-enable-auto-format-code nil
        lsp-bridge-enable-log nil
        lsp-bridge-enable-signature-help t
        lsp-bridge-python-lsp-server "jedi")
  )

(add-hook 'prog-mode-hook 'lsp-bridge-mode)
;; (global-lsp-bridge-mode)

(provide 'lt-lsp)
