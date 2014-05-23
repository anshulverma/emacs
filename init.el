(add-to-list 'load-path "~/emacs/lisp/")

(require 'emacs-type)

(load-library "packages") ;; make sure all the packages are installed
(load-library "custom") ;; customizations to the my emacs environment
(load-library "modes") ;; mode and configurations

 ;; startup "script" for when we've got a window system
(if-not-terminal
 (progn ;; start gnuserv, so apps can talk to us (e.g. p4, browsers)
 (when (or google
           (and winnt (not cygwin)))
 (require 'gnuserv)
 (setq gnuserv-frame (selected-frame))
 (gnuserv-start))
