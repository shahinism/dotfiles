;;; init.el -*- lexical-binding: t; -*-

;; Set some variables that must be configured before loading the package
(use-package evil
  :init
  (customize-set-variable 'evil-want-integration t)
  (customize-set-variable 'evil-want-keybinding nil)
  (customize-set-variable 'evil-want-C-i-jump nil)
  (customize-set-variable 'evil-respect-visual-line-mode t)
  :config

  ;; Load Evil and enable it globally
  (evil-mode 1)

  ;; Make C-g revert to normal state
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
  (global-set-key (kbd "C-M-u") 'universal-argument)

  ;; C-h is backspace in insert state
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Ensure these modes start in Emacs state
  (dolist (mode '(custom-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  )

(use-package undo-tree
  :after evil
  :init
  (customize-set-variable 'evil-undo-system 'undo-tree)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys)
  )

(use-package ace-window)
(use-package crux)

(use-package general
  :after evil ace-window crux
  :config

(general-evil-define-key 'normal 'global
 :prefix "SPC"
  "ff" 'find-file
  "fr" 'consult-recent-file
  "fs" 'save-buffer
  "bb" 'switch-to-buffer
  "gs" 'magit-status
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "w/" 'split-window-right
  "w-" 'split-window-below
  "wdd" 'delete-window
  "wda" 'ace-delete-window
  "wa" 'ace-window
  "ws" 'ace-swap-window
  "tt" 'lt/eshell-pop-toggle
  "br" 'crux-rename-file-and-buffer)
)

(use-package evil-easymotion
  :config
  (evilem-default-keybindings ",")
  )

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

(provide 'lt-evil)
