;; ----WHITESPACE----
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face
                         tabs
                         trailing
                         lines-tail
                         space-before-tab
                         newline
                         indentation
                         empty
                         space-after-tab))
(setq whitespace-line-column nil) ; this will make it same as fill-column
(setq-default whitespace-mode t)
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


;; ----SMEX----
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
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
