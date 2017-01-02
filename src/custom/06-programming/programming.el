;;; programming --- Summary
;;; Commentary:
;;; Code:

(semantic-mode 1)

(defun av/setup-programming-mode ()
  "Define common configurations for common programming modes."
  (interactive)
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t)
  (subword-mode +1)
  (set-fill-column 120)

  ;; use pretty symbols for commonly used words
  (-each '((">=" . ?≥)
           ("<=" . ?≤)
           ("!=" . ?≠)
           ("lambda" ?λ)
           ("defun" ?ƒ)
           ("function" ?ƒ))
    (lambda (mapping) (push mapping prettify-symbols-alist)))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode +1)

  ;; set face based on mode
  (setq buffer-face-mode-face '(:family "Source Code Pro"))
  (buffer-face-mode))

(define-minor-mode av/programming-mode
  "Set up a programming environment with proper key-bindings, useful modes, styles etc."
  :lighter " av/programming"
  :keymap '(([M-e] . sp-up-sexp))
  :init-value nil
  (if av/programming-mode
      ;; Turn on
      (av/setup-programming-mode)
    ;; Turn off
    (message "Turning off all modifications made by the av/programming mode is currently not supported")))

(-each '(emacs-lisp
         ruby
         js
         java
         clojure
         scala)
  (lambda (type)
         (let ((mode-hook (intern (s-concat (symbol-name type) "-mode-hook"))))
           (message (format "setting up '%s' programming environment" type))
           (add-hook mode-hook #'av/programming-mode))))

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
    (if (and char-at-point (char-equal ?} char-at-point))
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

;; setup `origami'
(require 'origami)

(global-origami-mode)

(defun av/folding (arg)
  "Fold code base don ARG."
  (interactive "P")
  (cond
   ((eq arg 1) (origami-close-all-nodes (current-buffer)))
   ((eq arg 2) (origami-open-all-nodes (current-buffer)))
   ((eq arg 3) (origami-close-node (current-buffer) (point)))
   ((eq arg 4) (origami-open-node (current-buffer) (point)))
   (t (error (concat "Invalid ARG: " arg)))))

(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (av/folding 1)))
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (av/folding 2)))
(global-set-key (kbd "M-<left>")
                (lambda () (interactive) (av/folding 3)))
(global-set-key (kbd "M-<right>")
                (lambda () (interactive) (av/folding 4)))

(global-set-key (kbd "C-c f o") 'av/folding)
(global-set-key (kbd "C-c f u") 'origami-undo)
(global-set-key (kbd "C-c f r") 'origami-redo)

(provide 'programming)
;;; programming.el ends here
