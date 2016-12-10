;;; programming --- Summary
;;; Commentary:
;;; Code:

(semantic-mode 1)

(defun custom-programming-modes-hook ()
  "Define common configurations for common programming modes."
   (make-local-variable 'column-number-mode)
   (column-number-mode t)
   (if window-system (hl-line-mode t))
   (idle-highlight-mode t)
   (subword-mode +1))

(add-hook 'emacs-lisp-mode-hook 'custom-programming-modes-hook)
(add-hook 'ruby-mode-hook       'custom-programming-modes-hook)
(add-hook 'js-mode-hook         'custom-programming-modes-hook)
(add-hook 'java-mode-hook       'custom-programming-modes-hook)
(add-hook 'clojure-mode-hook    'custom-programming-modes-hook)

;; eldoc mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(defun ted-frob-eldoc-argument-list (string)
  "Upcase and fontify STRING for use with `eldoc-mode'."
  (propertize (upcase string)
              'face 'font-lock-variable-name-face))
(setq eldoc-argument-case 'ted-frob-eldoc-argument-list)

;; enable yas snippets in programming modes
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; show pos tip for autocompletion option
(require 'pos-tip)

(defadvice popup-tip
    (around popup-pos-tip-wrapper (string &rest args) activate)
  (if (eq window-system 'x)
      (apply 'popup-pos-tip string args)
    ad-do-it))

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

(defun av/structured-programming-mode-hook ()
  "Set up key bindings for structured programming language hooks."
  (local-set-key (kbd "RET") 'av/auto-indent-method-maybe)
  (local-set-key (kbd "M-RET") 'av/nextline-and-indent)
  (subword-mode 1))

(add-hook 'java-mode-hook 'av/structured-programming-mode-hook)
(add-hook 'groovy-mode-hook 'av/structured-programming-mode-hook)
(add-hook 'scala-mode-hook 'av/structured-programming-mode-hook)

(provide 'programming)
;;; programming.el ends here
