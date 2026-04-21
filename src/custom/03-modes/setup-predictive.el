;;; setup-predictive --- Summary
;;; Commentary:
;;; Code:

(defvar av-predictive-dir (expand-file-name "predictive" av-lib-dir)
  "Base directory for predictive source.")

;; Predictive lives in a submodule that isn't initialized by default.
;; Skip silently if the user hasn't pulled it.
(when (file-exists-p (expand-file-name "predictive.el" av-predictive-dir))
  (add-to-list 'load-path av-predictive-dir)
  (add-to-list 'load-path (expand-file-name "latex" av-predictive-dir))
  (add-to-list 'load-path (expand-file-name "texinfo" av-predictive-dir))
  (add-to-list 'load-path (expand-file-name "html" av-predictive-dir))

  (require 'predictive)
  (require 'predictive-latex)
  (require 'predictive-texinfo)
  (require 'predictive-html)

  (set-default 'predictive-auto-add-to-dict t)
  (setq predictive-main-dict 'rpg-dictionary
        predictive-auto-learn t
        predictive-add-to-dict-ask nil
        predictive-use-auto-learn-cache nil
        predictive-which-dict t))

(provide 'setup-predictive)
;;; setup-predictive.el ends here
