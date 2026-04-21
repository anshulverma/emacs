;;; setup-smex --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Smex is only kept as a ranking backend for the helm-smex source
;; (see setup-helm.el). M-x itself is bound to helm-M-x by the helm
;; config; don't override it here.

(require 'smex)
(smex-initialize)
(setq-default smex-history-length 64)
;; Keep the built-in M-x reachable as an escape hatch.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'setup-smex)
;;; setup-smex.el ends here
