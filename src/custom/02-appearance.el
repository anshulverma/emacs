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
(require 'linum)
(require 'linum-off)

; line number format
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (if (display-graphic-p)
                            (format "%%%dd" w)
                          (format "%%%dd " w)) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
(global-linum-mode 1)

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
