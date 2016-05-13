;;; flycheck --- Summary
;;; Commentary:
;;; Code:

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-=") 'er/flycheck)

(defun er/add-text-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(mark-paragraph
                              mark-page))))

(add-hook 'text-mode-hook 'er/add-text-mode-expansions)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide '03-flycheck)
;;; 02-flycheck.el ends here
