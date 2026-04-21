;; appearance --- Summary
;;; Commentary:
;;; Code:

;; add my custom hook
(defvar av/theme-load-hook nil
  "Hook called after a theme is loaded.")

(defvar av/theme 'leuven
  "Name of the theme to initialize Emacs with.")

;; needed to display emojis
(set-fontset-font t nil "Symbola")

(defvar av/face-height 144
  "Font height to set for your enviroment.")
(set-face-attribute 'default nil :height av/face-height)

;; visible bel
(setf visible-bell t)

;; display file name in window title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
                 "Emacs: %b"))))

;; mouse settings
(set-mouse-color "black")

;; ----WHITESPACE----
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face
                         tabs
                         trailing
                         lines-tail
                         lines
                         space-before-tab
                         newline
                         indentation
                         space-after-tab
                         tab-mark))
(setq-default whitespace-mode t)
(setq-default show-trailing-whitespace t)
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '((space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]))) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)

;; ----LINE NUMBERS----
;; Use the built-in display-line-numbers-mode (Emacs 26+). The legacy
;; linum-mode is slow on large files and was removed upstream.
(setq-default display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)

;; Don't show line numbers in buffers where they add noise.
(dolist (mode-hook '(term-mode-hook
                     shell-mode-hook
                     eshell-mode-hook
                     org-mode-hook
                     org-agenda-mode-hook
                     Info-mode-hook
                     help-mode-hook
                     dired-mode-hook))
  (add-hook mode-hook (lambda () (display-line-numbers-mode -1))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; do not show startup screen
(setq inhibit-startup-screen t)

;; hide mode line when only a single buffer is open
(require 'hide-mode-line)

(defun av-hide-mode-line-hook()
  "Hides mode line."
  (hide-mode-line-update))

(add-hook 'prog-mode-hook 'av-hide-mode-line-hook)

(setq hide-mode-line t)

(provide '02-appearance)
;;; 02-appearance.el ends here
