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

;; indentation
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(electric-indent-mode 1)

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

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; show matching parenthesis
(show-paren-mode 1)

;; turn on elisp documentatin auto display
(defun ted-frob-eldoc-argument-list (string)
   "Upcase and fontify STRING for use with `eldoc-mode'."
   (propertize (upcase string)
               'face 'font-lock-variable-name-face))
(setq eldoc-argument-case 'ted-frob-eldoc-argument-list)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; change M-. to find function at point instead of find tag at point for elisp
(define-key emacs-lisp-mode-map
(kbd "M-.") 'find-function-at-point)

;; increase emacs GC threshold to ~20MB
(setq gc-cons-threshold 20000000)

;; save point between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")
