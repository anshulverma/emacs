;;; setup-smex --- Summary
;;; Commentary:
;;; Code:

(require 'smex)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; keep old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq-default smex-history-length 64)

(provide 'setup-smex)
;;; setup-smex.el ends here
