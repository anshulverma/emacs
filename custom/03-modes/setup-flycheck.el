;;; setup-flycheck --- Summary
;;; Commentary:
;;; Code:

(require 'flycheck)

(add-hook 'prog-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
