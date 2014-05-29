;; ----WHITESPACE----
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face tabs spaces trailing lines-tail space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
(setq whitespace-line-column nil) ; this will make it same as fill-column
(setq-default whitespace-mode t)
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
  '((space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]))) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
(global-set-key "\C-c_w" 'whitespace-mode)
(global-set-key "\C-c_t" 'whitespace-toggle-options)
(global-set-key "\C-c=w" 'global-whitespace-mode)
(global-set-key "\C-c=t" 'global-whitespace-toggle-options)
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)


;; ----FILL COLUMN----
(require 'column-marker)
(require 'fill-column-indicator)
(setq-default fill-column 100)
(add-hook 'after-change-major-mode-hook 'fci-mode) ; every file should have a fill-column
(setq fci-rule-width 2)
(setq fci-rule-color "dark gray")

;; markdown customization
 (add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook
                          'check-parens
                          nil t))))


;; ----LINE NUMBERS----
(require 'linum)
(global-linum-mode 1)
