;;; shell --- Summary
;;; Commentary:
;;; Code:

(if (equal (getenv "EMACS_SHELL") "1")
    (print "EMACS_SHELL is set; adding shell to your emacs session"
           ;; run shell in emacs
           (shell))
  (print "EMACS_SHELL is not set; skipping shell customization"))
(setenv "PAGER" "cat")

;; clear shell buffer
(defun clear-shell ()
  "Truncate contents of shell buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun shell-mode-setup()
  "Set up key bindings and other things specific to shell mode"
  (progn
    (local-set-key (kbd "C-x c") 'clear-shell)))
(setq shell-mode-hook 'shell-mode-setup)

;; fix ANSI color
(eval-after-load 'shell
  '(progn
     (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
     (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
     t))

(provide '03-shell)
;;; 03-shell.el ends here
