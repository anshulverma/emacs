(custom-set-variables
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes (quote ("5ace361b4ed7f0d1b0b7d72934446e1c61cded099db9f52125036f0a0bdea498"
                              default)))
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(sh-basic-offset 2)
 '(tab-width 2)
 '(blink-cursor-mode nil))
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
                         :overline nil :underline nil :slant normal :weight normal :height 128
                         :width normal :foundry "unknown" :family "Courier New"))))
 '(linum ((t (:background "gray20" :foreground "gray60" :family "Courier New"))))
 '(hl-line ((t (:background "gray16"))))
 '(popup-face ((t (:background "dim gray" :foreground "white smoke"))))
 '(mode-line ((t (:foreground "#030303" :background "#fdfdfd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 '(speedbar-file-face ((t (:foreground "gray88"))))
 '(speedbar-selected-face ((t (:background "gray80" :foreground "gray6"))))
 '(diff-removed ((t (:inherit diff-changed :background "#553333" :foreground "gray"))))
 '(region ((t (:background "gray24"))))
 '(font-lock-string-face ((t (:foreground "#bc8125")))))

;;; environment look and feel

; turn on syntax highlighting
(global-font-lock-mode 1)

; visible bel
(setf visible-bell t)

; turn on flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; use popup for flyspell correction
(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort (car (cdr (cdr poss))) 'string<)
                     (car (cdr (cdr poss)))))
         (cor-menu (if (consp corrects)
                       (mapcar (lambda (correct)
                                 (list correct correct))
                               corrects)
                     '()))
         (affix (car (cdr (cdr (cdr poss)))))
         show-affix-info
         (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                     (list
                                      (list (concat "Save affix: " (car affix))
                                            'save)
                                      '("Accept (session)" session)
                                      '("Accept (buffer)" buffer))
                                   '(("Save word" save)
                                     ("Accept (session)" session)
                                     ("Accept (buffer)" buffer)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu (mapcar
                (lambda (arg) (if (consp arg) (car arg) arg))
                base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
(eval-after-load "flyspell"
  '(progn
     (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

; use textual popup in terminal without window system
(defun flyspell-emacs-popup-choose (org-fun event poss word)
  (if (window-system)
      (funcall org-fun event poss word)
    (flyspell-emacs-popup-textual event poss word)))
(eval-after-load "flyspell"
  '(progn
     (defadvice flyspell-emacs-popup (around flyspell-emacs-popup-choose-advice)
       (flyspell-emacs-popup-choose))))

; display file name in window title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
                 "Emacs: %b"))))

;; set default width and onlyspaces mode
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(setq sh-basic-offset 2
      sh-indentation 2)

;; indentation
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq js-indent-level 2)

;; mouse settings
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

;;; editing customizations
(setq kill-whole-line t) ; kill entire line including RET

; copy line(s) function
(defun jao-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

; yank with indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode
                           lisp-mode
                           sh-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Indentation for python
;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; popup select window
(require 'popup)
(require 'popup-select-window)
(global-set-key (kbd "C-x o") 'popup-select-window)
(setq popup-select-window-window-highlight-face
      '(:background "navy"))
(setq popup-select-window-window-highlight-face
      '(:foreground "white" :background "navy"))
(setq popup-select-window-use-modeline-highlight nil)
(setq popup-select-window-use-buffer-highlight nil)
(setq popup-select-window-active-modeline-bgcolor "blue")
(setq popup-select-window-inactive-modeline-bgcolor "gray")
(setq popup-select-window-popup-windows 1)

;; fix ANSI colors in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; add stacktrace regexp for Javascript
(defun js-error-trace ()
  (add-to-list 'compilation-error-regexp-alist 'js-error-regexp)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(js-error-regexp " at [^/]*\\(/[^:]*\\):\\([0-9]*\\):\\([0-9]*\\))?$"
                                 1 2 3)))
(add-hook 'coffee-mode-hook 'js-error-trace)
(add-hook 'js-mode-hook 'js-error-trace)

;; customize speedbar
(require 'sr-speedbar)
(setq speedbar-frame-parameters
      '((minibuffer)
        (width . 24)
        (border-width . 10)
        (menu-bar-lines . 20)
        (tool-bar-lines . 30)
        (unsplittable . t)
        (left-fringe . 40)))
(setq speedbar-hide-button-brackets-flag nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images t)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-max-width 24)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-console 24)
(setq speedbar-directory-unshown-regexp "^$")

(when window-system
  (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
    (set-frame-width (selected-frame)
                     (+ (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)
  (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
    (sr-speedbar-recalculate-width)
    (set-frame-width (selected-frame)
                     (- (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))
(add-hook 'speedbar-mode-hook (lambda () (linum-mode -1)))

; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; create new buffer
(defun new-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "buffer")))
