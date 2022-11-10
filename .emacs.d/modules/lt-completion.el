;;; init.el -*- lexical-binding: t; -*-

;; ivy is splitted into 3 components: ivy, swiper and counsel
;; Installing counsel will install all of them to your Emacs
(leaf counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (counsel-mode 1)

  (global-set-key (kbd "C-s") 'swiper))

(leaf ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode))

(leaf helpful
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(leaf company
  :doc "Modular in-buffer completion framework"
  :url "http://company-mode.github.io/"
  :ensure t
  :hook (prog-mode-hook . company-mode)
  :bind
  ((:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("<tab>" . company-complete-common-or-cycle))
   (:company-search-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)))
  :custom
  (company-idle-delay  . 0)
  (company-echo-delay  . 0)
  (company-ignore-case . t)
  (company-selection-wrap-around . t)
  (company-minimum-prefix-length . 1)
  )

(leaf company-prescient
  :after company
  :ensure t
  :global-minor-mode company-prescient-mode)

(leaf counsel-projectile
  :ensure t)

(leaf all-the-icons-ivy
  :ensure t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(leaf ivy-rich
  :ensure t
  :config
  (require 'ivy-rich)
  (ivy-rich-mode 1))

(leaf all-the-icons-ivy
  :after ivy-rich
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  )

(leaf marginalia
  :doc "Explain details of the consult candidates"
  :url "https://github.com/minad/marginalia"
  :global-minor-mode marginalia-mode
  :ensure t
  :custom-face
  (marginalia-documentation . '((t (:foreground "#79a8ff")))))

(leaf embark
  :doc "Mini-Buffer Actions Rooted in Keymaps Resources"
  :url "https://github.com/oantolin/embark"
  :ensure t
  :bind*
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command . #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(setq-default abbrev-mode 1)

(provide 'lt-completion)
