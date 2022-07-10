;;; init.el -*- lexical-binding: t; -*-

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

(defun lt/eshell-pop-show (name)
  "Create a pop up window with eshell named NAME."
  (let* ((window (split-window
                  (frame-root-window)
                  '45
                  'below))
         (buffer (get-buffer name)))

    (select-window window)
    (if buffer
        (set-window-buffer window name)
      (progn
        (eshell window)
        (rename-buffer name)))
    ))

(defun lt/eshell-pop-hide (name)
  "Hide the existing pop up window with eshell named NAME."
  (let ((shell-buffer (get-buffer-window name)))
    (select-window shell-buffer)
    (bury-buffer)
    (delete-window)))

(defun lt/eshell-pop-toggle ()
  "Toggle eshell pop up window."
  (interactive)
  (let ((name "shell-buffer"))
    (if (get-buffer-window name)
        (lt/eshell-pop-hide name)
      (lt/eshell-pop-show name))
    ))

;; Configuration
(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'eshell-z)))

(provide 'lt-shell)
