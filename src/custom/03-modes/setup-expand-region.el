;;; setup-expand-region --- Summary
;;; Commentary:
;;; Code:

(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

(defun er/add-text-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(mark-paragraph
                              mark-page))))

(add-hook 'text-mode-hook 'er/add-text-mode-expansions)

(provide 'setup-expand-region)
;;; setup-expand-region.el ends here
