;;; setup-smartparens --- Summary
;;; Commentary:
;;; Code:

(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

(global-set-key (kbd "C-c C-f") 'sp-forward-sexp)
(global-set-key (kbd "C-c C-b") 'sp-forward-sexp)
(global-set-key (kbd "C-c C-l C-b") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-c C-l C-f") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-c C-t") 'sp-transpose-sexp)
(global-set-key (kbd "C-c C-p") 'sp-splice-sexp)

(provide 'setup-smartparens)
;;; setup-smartparens.el ends here
