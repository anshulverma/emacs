;;; setup-ace-jump --- Summary
;;; Commentary:
;;; Code:

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))

(provide 'setup-ace-jump)
;;; setup-ace-jump.el ends here
