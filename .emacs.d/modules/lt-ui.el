;;; init.el -*- lexical-binding: t; -*-

(use-package all-the-icons)
(use-package elisp-demos)

;;; Font
(defun lt/ui--set-default-font (spec)
  "Set the default font based on SPEC

SPEC is expected to be a plist with the same key names as
accepted by `set-default-attribute'."
  (when spec
    (apply 'set-face-attribute 'default nil spec)))

;; TODO set font only when availabe
(lt/ui--set-default-font '(:font "FiraCode Nerd Font" :weight regular :height 100))

;;; Modeline
(use-package doom-modeline
  :config

  (add-hook 'after-init-hook #'doom-modeline-mode)

  ;; Configure `doom-modeline'
  (customize-set-variable 'doom-modeline-height 15)
  (customize-set-variable 'doom-modeline-bar 6)
  (customize-set-variable 'doom-modeline-minor-modes t)
  (customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)
  )

;;;; Help Buffers

;; Make `describe-*' screens more helpful
(use-package helpful
  :config
  (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-symbol] #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Bind extra `describe-*' commands
  (global-set-key (kbd "C-h K") #'describe-keymap)
  )

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-zenburn t)
  )

;; which key help
(use-package which-key
  :config
  (which-key-mode)
  )

(provide 'lt-ui)
