;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'evil-easymotion)
(lt/install-package 'expand-region)

;; Configuration
(evilem-default-keybindings ",")
(global-set-key (kbd "C-=") 'er/expand-region)


(provide 'lt-navigation)
