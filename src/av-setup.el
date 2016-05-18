;;; av-setup.el --- Where all the magic begins

;;; Commentary:

;;; Code:

(require 'av-util)
(require 'f)

;; copy environment variables when running in window mode
(when (memq window-system '(mac ns))
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PS1")
    (exec-path-from-shell-copy-env "GEM_HOME")
    (exec-path-from-shell-copy-env "IRBC")
    (exec-path-from-shell-copy-env "MY_RUBY_HOME")
    (exec-path-from-shell-copy-env "rvm_env_string")
    (exec-path-from-shell-copy-env "rvm_ruby_string")
    (exec-path-from-shell-copy-env "GEM_PATH")
    (exec-path-from-shell-copy-env "RUBY_VERSION")))

;; load all files in custom folder while maintaining the order
(mapc 'load (f--files
             (expand-file-name "custom" av-src-dir)
             (s-matches? "^.*\.el$" it) t))

(provide 'av-setup)

;;; av-setup.el ends here
