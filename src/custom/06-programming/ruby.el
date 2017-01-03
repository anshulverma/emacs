;;; ruby --- Summary
;;; Commentary:
;;; Code:

(require 'rvm)
(require 'company)
(require 'rspec-mode)

;; use enhanced ruby mode for all ruby files
(add-to-list 'auto-mode-alist             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'eldoc-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; automatically load correct ruby version
(defadvice inf-ruby-console-auto
    (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; add robe as a company backend
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; load rspec snippets
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(provide 'ruby)
;;; ruby.el ends here
