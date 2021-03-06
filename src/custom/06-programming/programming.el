;;; programming --- Summary
;;; Commentary:
;;; Code:

(semantic-mode 1)

(defun av/make-pretty-symbol-list (chars-or-string)
  "Make a pretty symbol list based on CHARS-OR-STRING that can be used for function `prettify-symbols-mode'."
  (let ((chars (cond
                 ((stringp chars-or-string) (string-to-list chars-or-string))
                 ((listp chars-or-string) chars-or-string)
                 (t (error "Invalid argument %s" chars-or-string)))))
    (append (--mapcat (list it '(Br . Bl)) (butlast chars)) (last chars))))

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
  (-each `((">="          . ?⩾)
           ("<="          . ?⩽)
           ("!="          . ?≠)
           ("lambda"      . ?λ)
           ("defun"       . ?ƒ)
           ("defn"        . ?ƒ)
           ("function"    . ?ƒ)
           ("defn-"       . ,(av/make-pretty-symbol-list "ƒ-"))
           ("schema/defn" . ,(av/make-pretty-symbol-list "schema/ƒ"))
           ("->"          . (?\s (Bc . Bc) ?🠊))
           ("->>"         . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?🠊 (Bc . Bl) ?🠊)))
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
         enh-ruby
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

(provide 'programming)
;;; programming.el ends here
