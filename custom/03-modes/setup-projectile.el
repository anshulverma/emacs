;;; setup-projectile --- Summary
;;; Commentary:
;;; Code:

(require 'projectile)

(projectile-global-mode) ;; to enable in all buffers
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

(provide 'setup-projectile)
;;; setup-projectile.el ends here
