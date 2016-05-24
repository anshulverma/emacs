;;; editor --- Summary
;;; Commentary:
;;; Code:

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

;; show matching parenthesis
(show-paren-mode 1)
;; alternative is 'expression, 'parenthesis or 'mixed
(setq show-paren-style 'mixed)

;; editing customizations
(setq kill-whole-line t) ; kill entire line including RET

;; yank with indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode
                           lisp-mode
                           sh-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; auto fill
(auto-fill-mode -1)

;; fill column
(require 'column-marker)
(require 'fill-column-indicator)
(setq-default fill-column 100)
(add-hook 'after-change-major-mode-hook 'fci-mode) ; every file should have a fill-column
(setq fci-rule-width 2)
(setq fci-rule-color "dark gray")

(provide '05-editor)
;;; 05-editor.el ends here
