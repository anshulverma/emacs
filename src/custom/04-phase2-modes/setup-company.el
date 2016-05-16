;;; setup-company --- Summary
;;; Commentary:
;;; Code:

(require 'company)
(require 'company-dict)

;; Where to look for dictionary files. Default is ~/.emacs.d/dict
(setq company-dict-dir
      (concat user-emacs-directory "~/.emacs.d/dict"))

(add-hook 'after-init-hook 'global-company-mode)

(require 'company-auctex)
(company-auctex-init)

(setq tab-always-indent 'complete)

(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
;; (setq completion-cycle-threshold 5)

(after-load 'company
            (diminish 'company-mode "CMP")
            (define-key company-mode-map (kbd "M-/") 'company-complete)
            (define-key company-active-map (kbd "M-/") 'company-select-next)
            (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev))
            (company-flx-mode +1)
            (company-quickhelp-mode 1))
(global-set-key (kbd "M-C-/") 'company-complete)

(after-load 'company-quickhelp
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
(add-hook 'after-init-hook 'company-quickhelp-mode)

(provide 'setup-company)
;;; setup-company.el ends here