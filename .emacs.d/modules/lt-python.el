;;; init.el -*- lexical-binding: t; -*-

;; Packages
(lt/install-package 'anaconda-mode)
(lt/install-package 'blacken)
(lt/install-package 'numpydoc)
(lt/install-package 'pyvenv)

;; Functions
(defun python-doc ()
  (interactive)
  (setq-local dash-docs-docsets '("Python_3")))

;; Hooks
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook #'eldoc-mode)
(add-hook 'python-mode-hook #'pyenv-mode)
(add-hook 'python-mode-hook #'pyenv-tracking-mode)

(when (fboundp #'eglot-ensure)
  (add-hook 'python-mode-hook #'eglot-ensure))

(when (fboundp #'dash-docs-search)
  (add-hook 'python-mode-hook #'python-doc))

;;; Ananconda
;;  Move anaconda python installation directory to user var/
(customize-set-variable
 'anaconda-mode-installation-directory
 (expand-file-name "anaconda-mode" lt/config-var-dir))

;;; pyenv
;;  restart python when the virtual environment changes
(add-hook 'pyenv-post-activate-hooks #'pyenv-restart-python)

;; default to the commonly used "venv" folder for the virtual
;; environment
(customize-set-variable 'pyvenv-default-virtual-env-name "venv")

;;; Python mode
(customize-set-variable 'python-indent-guess-indent-offset-verbose nil)

;;; numpydoc
(customize-set-variable 'numpydoc-insert-examples-block nil)
(customize-set-variable 'numpydoc-template-long nil)

(provide 'lt-python)
