;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'evil)
(lt/install-package 'evil-collection)
(lt/install-package 'evil-nerd-commenter)
(lt/install-package 'undo-tree)
(lt/install-package 'general)
(lt/install-package 'ace-window)
(lt/install-package 'crux)

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-undo-system 'undo-tree)

;; Load Evil and enable it globally
(require 'evil)
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

(evil-collection-init)

;; Evil nerd commenter
(evilnc-default-hotkeys)

;; General
(require 'general)

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
  "pa" 'consult-ag
  "pb" 'project-switch-to-buffer
  "pd" 'project-find-dir
  "pf" 'project-find-file
  "pk" 'project-kill-buffers
  "pr" 'project-query-replace-regexp
  "pc" 'project-compile
  "pp" 'project-switch-project
  "d." 'consult-dash
  "e"  'consult-flycheck
  "is" 'consult-yasnippet
  "tt" 'lt/eshell-pop-toggle
  "ld" 'eglot-find-declaration
  "li" 'eglot-find-implementation
  "lt" 'eglot-find-typeDefinition
  "lr" 'eglot-rename
  "br" 'crux-rename-file-and-buffer)

(provide 'lt-evil)
