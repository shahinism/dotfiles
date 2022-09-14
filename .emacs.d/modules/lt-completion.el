;;; init.el -*- lexical-binding: t; -*-

;; ivy is splitted into 3 components: ivy, swiper and counsel
;; Installing counsel will install all of them to your Emacs
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (counsel-mode 1)

  (global-set-key (kbd "C-s") 'swiper))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode))

(use-package helpful
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package company
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook 'global-company-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2))

(use-package company-prescient
  :after company
  :ensure t
  :config
  (company-prescient-mode))

;(use-package company-box
;  :after company
;  :ensure t
;  :hook (company-mode . company-box-mode))

(use-package counsel-projectile
  :ensure t)

(use-package all-the-icons-ivy
  :ensure t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy-rich
  :ensure t
  :config
  (require 'ivy-rich)
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config (all-the-icons-ivy-rich-mode 1))

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(setq-default abbrev-mode 1)

(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer)

(use-package ivy-yasnippet
  :bind ("C-c y" . ivy-yasnippet))

(provide 'lt-completion)
