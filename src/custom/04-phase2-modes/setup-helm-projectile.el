;;; setup-helm-projectile --- Summary
;;; Commentary:
;;; Code:

(setq helm-projectile-fuzzy-match t)

(require 'helm-projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-off)

(provide 'setup-helm-projectile)
;;; setup-helm-projectile.el ends here
