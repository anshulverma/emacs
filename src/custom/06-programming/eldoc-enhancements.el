;;; eldoc-enhancements --- Summary
;;; Commentary:
;;; Code:

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(defun ted-frob-eldoc-argument-list (string)
  "Upcase and fontify STRING for use with `eldoc-mode'."
  (propertize (upcase string)
              'face 'font-lock-variable-name-face))
(setq eldoc-argument-case 'ted-frob-eldoc-argument-list)

;; highlight current argument in eldoc string
(defun eldoc-get-arg-index ()
  (save-excursion
    (let ((fn (eldoc-fnsym-in-current-sexp))
          (i 0))
      (unless (memq (char-syntax (char-before)) '(32 39)) ; ? , ?'
        (condition-case err
            (backward-sexp)             ;for safety
          (error 1)))
      (condition-case err
          (while (not (equal fn (eldoc-current-symbol)))
            (setq i (1+ i))
            (backward-sexp))
        (error 1))
      (max 0 i))))

(defun eldoc-highlight-nth-arg (doc n)
  (cond ((null doc) "")
        ((<= n 0) doc)
        (t
         (let ((i 0))
           (mapconcat
            (lambda (arg)
              (if (member arg '("&optional" "&rest"))
                  arg
                (prog2
                    (if (= i n)
                        (put-text-property 0 (length arg) 'face 'underline arg))
                    arg
                  (setq i (1+ i)))))
            (split-string doc) " ")))))

(defun av/eldoc-highlight-and-add-docstring (orig-fun sym &rest args)
  "Highlight the current argument in eldoc and append the function's docstring."
  (let* ((raw (apply orig-fun sym args))
         (highlighted (eldoc-highlight-nth-arg raw (eldoc-get-arg-index)))
         (doc (ignore-errors
                (eldoc-docstring-first-line
                 (cdr (help-split-fundoc (documentation sym t) sym))))))
    (if (and doc (not (equal doc "")))
        (concat highlighted
                (if (> (+ (length highlighted) (length doc) 4)
                       (frame-width))
                    "\n" "    ")
                doc)
      highlighted)))
(when (fboundp 'eldoc-get-fnsym-args-string)
  (advice-add 'eldoc-get-fnsym-args-string
              :around #'av/eldoc-highlight-and-add-docstring))

;; help in *help* buffer
(defvar context-help nil
  "Enable context help in *help* buffer.")

(defun rgr/toggle-context-help ()
  "Turn on or off the context help.
Note that if ON and you hide the help buffer then you need to
manually reshow it.  A double toggle will make it reappear"
  (interactive)
  (with-current-buffer (help-buffer)
    (unless (local-variable-p 'context-help)
      (set (make-local-variable 'context-help) t))
    (if (setq context-help (not context-help))
        (progn
          (if (not (get-buffer-window (help-buffer)))
              (display-buffer (help-buffer)))))
    (message "Context help %s" (if context-help "ON" "OFF"))))

(defun rgr/context-help ()
  "Display function or variable at point in *Help* buffer if visible.
Default behaviour can be turned off by setting the buffer local
context-help to false"
  (interactive)
  (let ((rgr-symbol (symbol-at-point))) ; symbol-at-point http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2B.el
    (with-current-buffer (help-buffer)
      (unless (local-variable-p 'context-help)
        (set (make-local-variable 'context-help) t))
      (if (and context-help (get-buffer-window (help-buffer))
               rgr-symbol)
          (if (fboundp rgr-symbol)
              (describe-function rgr-symbol)
            (if (boundp rgr-symbol) (describe-variable rgr-symbol)))))))

(defun av/eldoc-show-c-tag (orig-fun &rest args)
  "Trigger `rgr/context-help' in lisp-like modes before eldoc displays info."
  (when (memq major-mode '(emacs-lisp-mode
                           lisp-interaction-mode
                           apropos-mode))
    (rgr/context-help))
  (apply orig-fun args))
(when (fboundp 'eldoc-print-current-symbol-info)
  (advice-add 'eldoc-print-current-symbol-info
              :around #'av/eldoc-show-c-tag))

(global-set-key (kbd "C-c h") 'rgr/toggle-context-help)

(provide 'eldoc-enhancements)
;;; eldoc-enhancements.el ends here
