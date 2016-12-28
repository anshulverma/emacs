;;; global --- Summary
;;; Commentary:
;;; Code:

;; FIX-ME: a test macro that will be replaced with something useful
(fset 'test-macro
      "\C-e this is a test\342\342\342\342; \C-a\C-e\342\342\342\342\C-b\C-b\C-b")

(put 'test-macro 'kmacro t)

(provide '02-macros)
;;; 02-macros.el ends here
