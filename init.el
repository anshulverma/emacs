(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/lib/")

(mapc 'load (f--files "~/.emacs.d/custom" (s-matches? "^.*\.el$" it) t))
