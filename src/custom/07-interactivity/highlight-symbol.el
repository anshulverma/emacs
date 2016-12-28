;;; highlight-symbol --- Summary
;;; Commentary:

;;; Code:

(require 'highlight-symbol)

(add-hook 'av/programming-mode-hook
          (lambda ()
            (setq-local highlight-symbol-on-navigation-p t)
            (setq-local highlight-symbol-idle-delay 0.2)
            (local-set-key [(control f3)] 'highlight-symbol)
            (local-set-key [f3] 'highlight-symbol-next)
            (local-set-key [(shift f3)] 'highlight-symbol-prev)
            (local-set-key [(meta f3)] 'highlight-symbol-query-replace)
            (highlight-symbol-mode +1)))

(provide 'highlight-symbol)
;;; highlight-symbol.el ends here
