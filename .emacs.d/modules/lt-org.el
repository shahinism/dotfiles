;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'org-appear)
(lt/install-package 'org-bullets)

(require 'org)
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
(add-hook 'org-mode-hook 'org-appear-mode)

;; disable auto-pairing of "<" in org mode
(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; Fancy bullets
(add-hook 'org-mode-hook #'org-bullets-mode)

(provide 'lt-org)
