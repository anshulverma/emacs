;;; setup-projectile --- Summary
;;; Commentary:
;;; Code:

(require 'projectile)

(projectile-mode +1) ;; to enable in all buffers
;; `projectile-global-mode' was an alias for `projectile-mode' and has been
;; removed upstream; calling it fails init on a fresh ELPA install.
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

;; this seems to be broken at the moment
;; https://github.com/bbatsov/projectile/issues/92
(add-to-list 'projectile-globally-ignored-files "*.~undo-tree~")

(provide 'setup-projectile)
;;; setup-projectile.el ends here
