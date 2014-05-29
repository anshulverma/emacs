(custom-set-variables
 '(custom-enabled-themes (quote (wombat)))
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(sh-basic-offset 2)
 '(tab-width 2))
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
                         :overline nil :underline nil :slant normal :weight normal :height 98
                         :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(linum ((t (:background "dim gray" :forground "gold" :family "DejaVu Sans Mono")))))

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;; set default width and onlyspaces mode
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(setq sh-basic-offset 2
      sh-indentation 2)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

(set-mouse-color "black")

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

;; smart commenting with M-;
 (defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
        (when (region-active-p)
          (setq start (save-excursion
                        (goto-char (region-beginning))
                        (beginning-of-line)
                        (point))
                end (save-excursion
                      (goto-char (region-end))
                      (end-of-line)
                      (point))))
        (comment-or-uncomment-region start end)
        (next-line)))
(global-set-key "\M-;" 'comment-eclipse)
