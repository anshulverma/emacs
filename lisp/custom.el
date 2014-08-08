(custom-set-variables
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes (quote ("5ace361b4ed7f0d1b0b7d72934446e1c61cded099db9f52125036f0a0bdea498"
                              default)))
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(sh-basic-offset 2)
 '(tab-width 2)
 '(blink-cursor-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "namail.corp.adobe.com")
 '(smtpmail-smtp-service 25)
 '(user-full-name "Anshul Verma")
 '(user-mail-address "ansverma@adobe.com")
 '(global-font-lock-mode t))
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
                         :overline nil :underline nil :slant normal :weight normal :height 96
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
 '(font-lock-string-face ((t (:foreground "#bc8125"))))
 '(web-mode-block-face ((t (:background "dark slate gray")))))

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

; reuse frame for buffers
(setq-default display-buffer-reuse-frames t)

; set up ruby environment
(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation)
                               (define-key ruby-mode-map (kbd "M-r") 'run-rails-test-or-ruby-buffer))))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

; yassnippet configuration
;; Completing point by some yasnippet key
(require 'yasnippet)
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

(add-to-list 'yas-snippet-dirs (concat basedir "snippets"))

;; ----REVEL.JS ORG----
(setq org-reveal-root (concat basedir "../reveal.js"))

; turn on cdlatex in org mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             ;; beamer class, for presentations
             '("beamer"
               "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

               ("\\section{%s}" . "\\section*{%s}")

               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; letter class, for formal letters

(add-to-list 'org-export-latex-classes

             '("letter"
               "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

; set default method for tramp
(setq tramp-default-method "ssh")

; set up smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;; no confirmation prompt when killing a buffer opened by emacsclient
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
