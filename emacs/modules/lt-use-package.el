;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'use-package)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(provide 'lt-use-package)
