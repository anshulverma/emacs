;;; pig --- Summary
;;; Commentary:
;;; Code:

(setq pig-executable "/usr/local/bin/pig")
(setq pig-executable-options '("-x" "local"))
(setq pig-executable-prompt-regexp "^grunt> ")
(setq pig-indent-level 4)
(setq pig-version "0.16.0")

;; yas snippets for pig
(autoload 'pig-snippets-initialize "pig-snippets" nil nil nil)
(eval-after-load 'yasnippet '(pig-snippets-initialize))

(provide 'pig)
;;; pig.el ends here
