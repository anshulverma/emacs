;;; setup-projectile --- Summary
;;; Commentary:
;;; Code:

(require 'projectile)

(projectile-global-mode) ;; to enable in all buffers
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

;; this seems to be broken at the moment
;; https://github.com/bbatsov/projectile/issues/92
(add-to-list 'projectile-globally-ignored-files "*.~undo-tree~")

(provide 'setup-projectile)
;;; setup-projectile.el ends here
