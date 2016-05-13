;;; setup-auto-complete --- Summary
;;; Commentary:
;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(ac-config-default)
(ac-flyspell-workaround)

(setq ac-delay 0.1 ac-auto-start 1)

;; auto-complete binding
(global-set-key [C-tab] 'ac-start)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here
