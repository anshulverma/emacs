;;; appearance --- Summary
;;; Commentary:
;;; Code:

(load-theme 'zenburn t)

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
(setq whitespace-line-column nil) ; this will make it same as fill-column
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

; line number format
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (if (display-graphic-p)
                            (format "%%%dd" w)
                          (format "%%%dd " w)) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
(global-linum-mode 1)

;; position and size window automatically based on display resolution
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 200)
                                      (frame-char-height)))))))
(set-frame-size-according-to-resolution)

(provide '02-appearance)
;;; 02-appearance.el ends here
