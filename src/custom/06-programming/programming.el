;;; programming --- Summary
;;; Commentary:
;;; Code:

(semantic-mode 1)

(defun custom-programming-modes-hook ()
  "Define common configurations for common programming modes."
   (make-local-variable 'column-number-mode)
   (column-number-mode t)
   (if window-system (hl-line-mode t))
   (idle-highlight-mode t))

(add-hook 'emacs-lisp-mode-hook 'custom-programming-modes-hook)
(add-hook 'ruby-mode-hook       'custom-programming-modes-hook)
(add-hook 'js-mode-hook         'custom-programming-modes-hook)
(add-hook 'java-mode-hook       'custom-programming-modes-hook)

;; enable yas snippets in programming modes
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; auto indent on opening brace
(require 'cc-mode)
(defun av/auto-indent-method ()
  "Automatically indent a method by adding two newlines.
Puts point in the middle line as well as indent it by correct amount."
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (forward-line -1)
  (c-indent-line-or-region))

(defun av/auto-indent-method-maybe ()
  "Check if point is at a closing brace then auto indent."
  (interactive)
  (let ((char-at-point (char-after (point))))
    (if (char-equal ?} char-at-point)
        (av/auto-indent-method)
      (newline-and-indent))))

(defun av/nextline-and-indent ()
  "Go to the end of line then `av/auto-indent-method-maybe'."
  (interactive)
  (end-of-line 1)
  (av/auto-indent-method-maybe))

(define-key java-mode-map (kbd "RET") 'av/auto-indent-method-maybe)
(define-key java-mode-map (kbd "M-RET") 'av/nextline-and-indent)

(provide 'programming)
;;; programming.el ends here
