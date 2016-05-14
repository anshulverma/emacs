;;; setup-flycheck --- Summary
;;; Commentary:
;;; Code:

(require 'flycheck)

(add-hook 'prog-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(flycheck-pos-tip-mode))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
