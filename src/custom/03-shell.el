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

;; enable smartparens in shell mode
(add-hook 'eshell-mode-hook (lambda() (smartparens-mode +1)))

;; set up eshell prompt
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun shk-eshell-prompt ()
  (let ((header-bg "#777"))
    (concat
     (with-face (eshell/pwd) :background header-bg)
     (with-face
      (or (ignore-errors (format " (%s)" (vc-responsible-backend default-directory))) "")
      :background header-bg)
     (with-face "\n" :background header-bg)
     (with-face user-login-name :foreground "blue")
     "@"
     (with-face "localhost" :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)

(provide '03-shell)
;;; 03-shell.el ends here
