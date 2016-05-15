;;; av-packages.el --- Where all the magic begins

;;; Commentary:

;;; Code:

(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(provide 'av-packages)

;;; av-packages.el ends here
