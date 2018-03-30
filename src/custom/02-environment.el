;;; environment --- Summary
;;; Commentary:
;;; Code:

;;; extensions to dired
(require 'dired+)

;;; make sure SSL/TLS is supported
(require 'tls)

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

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "echo $PATH")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(provide '02-environment)
;;; 02-environment.el ends here
