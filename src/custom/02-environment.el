;;; environment --- Summary
;;; Commentary:
;;; Code:

;; Built-in dired has gained most of what the vendored dired+ provided
;; (hide-details, toggle-marks, subtree navigation via dired-subtree
;; from MELPA if desired). The old dired+.el was fragile on newer
;; Emacs, so it's no longer loaded here.

;;; make sure SSL/TLS is supported
(require 'tls)

;;; ----MAC OSX----
;; exec-path-from-shell (loaded in av-setup) populates PATH from the
;; user's shell, so `executable-find' sees both /usr/local/bin (Intel)
;; and /opt/homebrew/bin (Apple Silicon) without hard-coded prefixes.
(when (eq system-type 'darwin)
  (when-let ((ispell (executable-find "ispell")))
    (setq ispell-program-name ispell))

  (with-eval-after-load 'markdown-mode
    (when-let ((md (or (executable-find "markdown")
                       (executable-find "multimarkdown")
                       (executable-find "pandoc"))))
      (setq markdown-command md)))

  ;; set function key as hyper
  (setq ns-function-modifier 'hyper))

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "echo $PATH")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(provide '02-environment)
;;; 02-environment.el ends here
