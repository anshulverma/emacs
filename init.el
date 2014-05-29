(add-to-list 'load-path "~/emacs/lisp/")

(require 'emacs-type)

(load-library "packages") ;; make sure all the packages are installed
(load-library "custom") ;; customizations to the my emacs environment
(load-library "modes") ;; mode and configurations
(load-library "key_bindings") ;; custom key bindings
(load-library "custom_shell") ;; shell loading and configuration
