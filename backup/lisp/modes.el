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

;; ----FILL COLUMN----
(require 'column-marker)
(require 'fill-column-indicator)
(setq-default fill-column 100)
(add-hook 'after-change-major-mode-hook 'fci-mode) ; every file should have a fill-column
(setq fci-rule-width 2)
(setq fci-rule-color "dark gray")
(add-hook 'coffee-mode-hook
          (lambda ()
            (setq fill-column 80)))

;; markdown customization
(add-hook 'markdown-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook
                        'check-parens
                        nil t))))

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

;; ----IDO MODE----
(ido-mode 1)
(setq ido-use-faces nil
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

;; use flx-ido for better name matching
(require 'flx-ido)
(require 'ido-vertical-mode)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; ----SMEX----
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; keep old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq-default smex-history-length 64)

;; ----PROJECTILE----
(require 'projectile)
(projectile-global-mode) ;; to enable in all buffers
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

; perspective mode integration
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-c C-s") 'projectile-persp-switch-project)
(persp-mode 1)

;; ----WINDOW NUMBER----
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;; ----UNDO TREE----
(require 'undo-tree)
(global-undo-tree-mode)

;; ----ACE JUMP---
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; ----DELETE SELECTION----
(delete-selection-mode)

;; ----MAGIT----
(eval-after-load 'magit '(require 'setup-magit))

;; ----COLUMN NUMBER----
(setq column-number-mode 1)

;; ----COFFEE MODE----
(defun coffee-custom ()
  "coffee-mode-hook"
  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))
(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

; go to corresponding point in js after compilation
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

(add-hook 'coffee-mode-hook 'smartparens-mode)

;; ----AUTO COMPLETE----
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "../dict")

(ac-config-default)
(ac-flyspell-workaround)

(setq ac-delay 0.1
      ac-auto-start 1)

; enable auto complete for specific modes
(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'ac-modes 'java-mode)

;; ----POWERLINE----
(powerline-default-theme)
(setq powerline-default-separator (quote arrow-fade))

;; ----SPEEDBAR----
(require 'sr-speedbar)
(require 'speedbar-extension)
(require 'projectile-speedbar)

(defun speedbar-setup-hook ()
  "Set up properties in speedbar mode"
  (progn
    (set-fill-column 0)))
(add-hook 'speedbar-mode-hook 'speedbar-setup-hook)
(add-hook 'speedbar-mode-hook 'fci-mode)
(setq speedbar-vc-do-check nil)
(setq projectile-speedbar-enable nil)

;; ----HL-LINE----
(global-hl-line-mode)

;; ----SUB-WORD----
(add-hook 'prog-mode-hook 'subword-mode)

;; ----FLYCHECK----
(require 'setup-flycheck)

;; ----CUA----
(cua-mode t)

;; ----RIARI----
(require 'rinari)

;; ----ROBE----
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(add-hook 'robe-mode-hook 'auto-complete-mode)

;; ----ENHANCED-RUBY-MODE----
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'smartparens-mode)

;; ----HIGHLIGHT-INDENTATION----
(add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'enh-ruby-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'web-mode-hook 'highlight-indentation-current-column-mode)

;; ----DIMINISH----
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
(diminish 'global-whitespace-mode)
(diminish 'whitespace-mode)
(diminish 'undo-tree-mode)
(diminish 'ace-jump-mode)
(diminish 'projectile-mode)
(diminish 'auto-complete-mode)
(diminish 'eldoc-mode)
(diminish 'yas-minor-mode)
(diminish 'visual-line-mode)

(defun diminish-prog-modes ()
  "Diminish all the unwanted modes from prog modes"
  (progn
    (if (member 'flyspell-mode minor-mode-list)
        (diminish 'flyspell-mode))))
(add-hook 'emacs-lisp-mode-hook 'diminish-prog-modes)

;; ----WEB-MODE----
(require 'setup-web-mode)

;; ----CSV----
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-nav-mode))
;; (autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)

;; ----ORG-REVEAL----
(require 'ox-reveal)

;; ----ORG-LATEX----
(require 'ox-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;; ----YASNIPPET----
(require 'yasnippet)
(yas-global-mode 1)

;; ----GNUPLOT----
(require 'gnuplot-mode)

(add-to-list 'auto-mode-alist '("\\.gnuplot$" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.plt$" . gnuplot-mode))

;; ----ORG-MIME----
(require 'org-mime)
(setq org-mime-library 'mml)

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

;; ----DOCKER FILE----
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ----MULTIPLE CURSORS----
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-.") 'mc/edit-lines)  ; cursor on each line in selection
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)

;; ----GROOVY----
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; ----OCTAVE----
(add-to-list 'auto-mode-alist '("\.m$" . octave-mode))

(add-hook 'octave-mode-hook 'auto-complete-mode)

;; ----RVM----
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;; ----ECHO KEYS----
(require 'echo-keys)


;; ----JADE----
(add-to-list 'auto-mode-alist '("\.jade$" . jade-mode))


(provide 'modes)
;;; modes.el ends here
