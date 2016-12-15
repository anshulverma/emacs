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

(defadvice eldoc-get-fnsym-args-string (around highlight activate)
  ""
  (setq ad-return-value (eldoc-highlight-nth-arg ad-do-it
                                                 (eldoc-get-arg-index))))

;; add docstring to eldoc
(defadvice eldoc-get-fnsym-args-string (after add-dacstring (sym)
                                              activate compile)
  "Add a doc string to ElDoc's modeline information."
  (let ((doc (eldoc-docstring-first-line
              (cdr (help-split-fundoc (documentation sym t) sym)))))
    (when (and doc (not (equal doc "")))
      (setq ad-return-value
            (concat ad-return-value
                    (if (> (+ (length ad-return-value) (length doc) 4)
                           (frame-width)) "\n" "    ")
                    doc))))
  ad-return-value)

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

(defadvice eldoc-print-current-symbol-info
    (around eldoc-show-c-tag activate)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (rgr/context-help) ad-do-it)
   ((eq major-mode 'lisp-interaction-mode) (rgr/context-help) ad-do-it)
   ((eq major-mode 'apropos-mode) (rgr/context-help) ad-do-it)
   (t ad-do-it)))

(global-set-key (kbd "C-c h") 'rgr/toggle-context-help)

(provide 'eldoc-enhancements)
;;; eldoc-enhancements.el ends here
