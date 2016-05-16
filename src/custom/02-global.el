;;; global --- Summary
;;; Commentary:
;;; Code:

;; Fancier dired display
(require 'dired-details+)

;; turn on syntax highlighting
(global-font-lock-mode 1)

;; how long lines are handled.  This
;; appears to wrap long lines visually,
;; but not add line-returns
(global-visual-line-mode 1)

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

;; common-lisp functions
(require 'cl)

;; When you visit a file, point goes to the
;; last place where it was when you
;; previously visited the same file.
(require 'saveplace)

;; find-file-at-point
(require 'ffap)

;; overrides Emacs' default mechanism for
;; making buffer names unique (using suffixes
;; like <2>, <3> etc.) with a more sensible
;; behaviour which use parts of the file
;; names to make the buffer names
;; distinguishable.
(require 'uniquify)

;; translates ANSI SGR (Select Graphic
;; Rendition) escape sequences like "Esc [ 30
;; m" into EmacsOverlays, TextProperties, or
;; XEmacsExtents with face colours, bold,
;; etc.
(require 'ansi-color)

;; diminish keeps the modeline tidy
(require 'diminish)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; disable backup file creation
(setq backup-inhibited t)

;; answer with y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; http://emacsredux.com/blog/2013/04/05/recently-visited-files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
(setq save-place-file
      (expand-file-name "user/saved-places" av-basedir))
(global-set-key (kbd "<f7>") 'helm-recentf)

;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name (expand-file-name "abbrev_defs" av-basedir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)

(provide '02-global)
;;; 02-global.el ends here