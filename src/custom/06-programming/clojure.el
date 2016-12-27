;;; clojure --- Summary
;;; Commentary:
;;; Code:

;;; setup `cider'
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'subword-mode)

;;; setup `clj-refactor'
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  "Setup clojure mode settings."
  (interactive)
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq cider-mode-line-show-connection nil)

(provide 'clojure)
;;; clojure.el ends here
