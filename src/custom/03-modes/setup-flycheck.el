;;; setup-flycheck --- Summary
;;; Commentary:
;;; Code:

(require 'flycheck)

(add-hook 'prog-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(flycheck-pos-tip-mode))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
        :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
        :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

(global-set-key (kbd "C-c ! !") 'hydra-flycheck/body)

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
