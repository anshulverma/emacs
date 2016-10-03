;;; setup-markdown --- Summary
;;; Commentary:
;;; Code:

(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(autoload 'gfm-mode "gfm-mode" "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(provide 'setup-markdown)
;;; setup-markdown.el ends here
