;;; init.el -*- lexical-binding: t; -*-

; Configuration
;; Org
(use-package org
  :config
  (require 'org-tempo) ;; enable org templates; by default it's disabled
  ;; on Org > 9.2, more info:
  ;; https://emacs.stackexchange.com/a/46992

  (setq org-use-speed-commands t
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  ;; Return or left-click with mouse should follow links
  (customize-set-variable 'org-return-follows-link t)
  (customize-set-variable 'org-mouse-1-follows-link t)

  ;; Display links as the description provided
  (customize-set-variable 'org-descriptive-links t)

  ;; Hide markup markers
  (customize-set-variable 'org-hide-emphasis-markers t)

  ;; disable auto-pairing of "<" in org mode
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  )

(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  )

;; Fancy bullets
(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
)

;; Org-roam
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t
        org-directory (concat (getenv "HOME") "/Documents/notes/")
        org-roam-directory (file-truename org-directory))

  (unless (f-directory? org-roam-directory)
    (make-directory org-roam-directory t))

  :config
  (org-roam-setup)
  )

;; Deft
(use-package deft
  :config
  (setq deft-directory org-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  )
;; Org Download
(use-package org-download
  :config
  (setq org-download-method 'directory
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d-%H%M%S"
        org-image-actual-width t
        org-download-screenshot-method "flameshot gui --raw > %s")

  (customize-set-variable 'org-download-image-dir "images")
  )

;; Evil Keybindings
(when (fboundp #'general-evil-define-key)
  (general-evil-define-key 'normal 'org-mode-map
    :prefix "SPC"
    "ns" 'org-download-screenshot
    "ni" 'org-roam-node-insert
    "no" 'org-id-get-create
    "nt" 'org-roam-tag-add
    "na" 'org-roam-alias-add
    "nl" 'org-roam-buffer-toggle)

  (general-evil-define-key 'normal 'global
    :prefix "SPC"
    "nf" 'org-roam-node-find
    "nr" 'org-roam-node-random
    "nd" 'deft)
  )

(provide 'lt-note)
