;;; ruby --- Summary
;;; Commentary:
;;; Code:

(require 'inf-ruby)
(require 'rvm)
(require 'company)
(require 'rspec-mode)
(require 'ruby-tools)
(require 'ruby-refactor)

;; use enhanced ruby mode for all ruby files
(add-to-list 'auto-mode-alist             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'eldoc-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

(defun av/activate-rvm-for-robe (&rest _)
  "Automatically load correct ruby version before starting the ruby console."
  (rvm-activate-corresponding-ruby))
(advice-add 'inf-ruby-console-auto :before #'av/activate-rvm-for-robe)

;; add robe as a company backend
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; load rspec snippets
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; switch the compilation buffer mode with C-x C-q (useful when interacting with a debugger)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(provide 'ruby)
;;; ruby.el ends here
