;;; init.el -*- lexical-binding: t; -*-

;;; Vertico
;;  Vertical completion in minibuffer
(use-package vertico
  :config
  (require 'vertico-directory)

  ;; Cycle through the result when on edges
  (customize-set-variable 'vertico-cycle t)

  (vertico-mode 1)
  )

;;; Martinalia
;;  Help text in minibuffer completion
(use-package marginalia
  :config
  (customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode 1)
  )

;;; Consult
(use-package consult
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
)

;;; Orderless
;;  Better fuzzy matching
(use-package orderless
  :config
  (customize-set-variable 'completion-styles '(orderless))
  (customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))
  )

;;; Embark
(use-package embark
  :config

  (global-set-key [remap describe-bindings] #'embark-bindings)
  (global-set-key (kbd "C-.") 'embark-act)
  )

(use-package embark-consult
  :after embark consult)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Corfu
;; Setup corfu for popup like completion
(use-package corfu
  :config
  (customize-set-variable 'corfu-cycle t) ; Allows cycling through candidates
  (customize-set-variable 'corfu-auto t)  ; Enable auto completion
  (customize-set-variable 'corfu-auto-prefix 2) ; Complete with less prefix keys
  (customize-set-variable 'corfu-auto-delay 0.0) ; No delay for completion
  (customize-set-variable 'corfu-echo-documentation 0.25) ; Echo docs for current completion option

  (global-corfu-mode 1)
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))
  )

(use-package corfu-doc
  :after corfu)

;;; Cape
;;  Better completion at point
(use-package cape
  :config
  ;; Useful default completions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete doesn't write to the buffer and behaves like
  ;; a pure `completion-at-point-functions`.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

;; Kind Icon
(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(provide 'lt-completion)
