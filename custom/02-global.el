;;; global --- Summary
;;; Commentary:
;;; Code:

;; Fancier dired display
(require 'dired-details+)

;; turn on syntax highlighting
(global-font-lock-mode 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; reuse frame for buffers
(setq-default display-buffer-reuse-frames t)

;; increase emacs GC threshold to ~20MB
(setq gc-cons-threshold 20000000)

(custom-set-variables
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(sh-basic-offset 2)
 '(tab-width 2)
 '(blink-cursor-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-service 25)
 '(user-full-name "Anshul Verma"))

(require 'info-look)

(provide '02-global)
;;; 02-global.el ends here
