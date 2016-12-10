;;; clojure --- Summary
;;; Commentary:
;;; Code:

(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'company-mode)

(provide 'clojure)
;;; clojure.el ends here
