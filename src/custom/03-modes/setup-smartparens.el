;;; setup-smartparens --- Summary
;;; Commentary:
;;; Code:

(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

(global-set-key (kbd "C-c C-p C-n") 'sp-forward-sexp)
(global-set-key (kbd "C-c C-p C-p") 'sp-backward-sexp)
(global-set-key (kbd "C-c C-p C-b") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-c C-p C-f") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-c C-p C-t") 'sp-transpose-sexp)
(global-set-key (kbd "C-c C-p C-l") 'sp-splice-sexp)

(global-set-key (kbd "C-)") 'sp-up-sexp)

(provide 'setup-smartparens)
;;; setup-smartparens.el ends here
