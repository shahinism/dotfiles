;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'shell-pop)
(lt/install-package 'eshell-z)

;; Functions

(defun lt/shell-has-sudo-p ()
  "Check if the current command on eshell prompt, starts with sudo."
  (let ((command-string (buffer-substring-no-properties
                         (progn (eshell-bol) (point)) (point-max))))
    (string-match-p "^sudo " command-string)))

(defun lt/shell-sudo-add ()
  (progn
    (eshell-bol)
    (insert "sudo ")))

(defun lt/shell-sudo-pop ()
  (progn
    (eshell-bol)
    (while (re-search-forward "sudo " nil t)
      (replace-match "" t nil))))

(defun lt/shell-sudo-toggle ()
  "Toggle sudo with the current command"
  (interactive
   (save-excursion
       (if (lt/shell-has-sudo-p)
           (lt/shell-sudo-pop)
         (lt/shell-sudo-add)))
     ))

;; Configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(shell-pop-default-directory "~/")
 '(shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-size 30)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom")
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-restore-window-configuration t)
 '(shell-pop-cleanup-buffer-at-process-exit t))

(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'eshell-z)))

(provide 'lt-shell)
