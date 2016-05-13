;;; programming --- Summary
;;; Commentary:
;;; Code:

(defun custom-programming-modes-hook ()
   (make-local-variable 'column-number-mode)
   (column-number-mode t)
   (if window-system (hl-line-mode t))
   (idle-highlight-mode t))

 (add-hook 'emacs-lisp-mode-hook 'custom-programming-modes-hook)
 (add-hook 'ruby-mode-hook 'custom-programming-modes-hook)
 (add-hook 'js-mode-hook 'custom-programming-modes-hook)

(provide '04-programming)
;;; 04-programming.el ends here

