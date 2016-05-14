;;; package --- Summary

;; Set up and configure flycheck mode
;;

;;; Commentary:

;; To use:
;; (require 'setup-flycheck)
;;

;;; Code:

(require 'flycheck)
(require 'flycheck-pos-tip)

;; (eval-after-load 'flycheck
  ;; '(custom-set-variables
    ;; '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(with-eval-after-load 'flycheck '((flycheck-pos-tip-mode)
                                  (add-hook 'prog-mode-hook 'flycheck-mode)))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
