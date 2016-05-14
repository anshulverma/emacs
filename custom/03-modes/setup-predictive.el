;;; setup-predictive --- Summary
;;; Commentary:
;;; Code:

;; predictive install location
(add-to-list 'load-path "~/.emacs.d/predictive/")

;; dictionary locations
(add-to-list 'load-path "~/.emacs.d/predictive/latex/")
(add-to-list 'load-path "~/.emacs.d/predictive/texinfo/")
(add-to-list 'load-path "~/.emacs.d/predictive/html/")

;; (autoload 'predictive-mode "~/.emacs.d/predictive/predictive"
;;   "Turn on Predictive Completion Mode." t)

;; (autoload 'predictive-setup-latex "~/.emacs.d/predictive/latex/")
;; (autoload 'predictive-setup-texinfo "~/.emacs.d/predictive/texinfo/")
;; (autoload 'predictive-setup-html "~/.emacs.d/predictive/html/")

(require 'predictive)

(require 'predictive-latex)
(require 'predictive-texinfo)
(require 'predictive-html)

(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'rpg-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

(provide 'setup-predictive)
;;; setup-predictive.el ends here
