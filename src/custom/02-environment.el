;;; environment --- Summary
;;; Commentary:
;;; Code:

;;; Fancier dired display
(require 'dired-details+)

;;; ----MAC OSX----
(if (eq system-type 'darwin)
    (progn
      ;; ispell is hard to find in emacs
      (setq ispell-program-name "/usr/local/bin/ispell")

      ;; make COMMAND key function as CTRL
      ;; (setq mac-command-modifier 'control)

      ;; markdown seems to be hard to find too
      (defun markdown-custom ()
        "markdown-mode-hook"
        (setq markdown-command "/usr/local/bin/markdown"))
      (add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

      ;; set function key as hyper
      (setq ns-function-modifier 'hyper)))

(provide '02-environment)
;;; 02-environment.el ends here
