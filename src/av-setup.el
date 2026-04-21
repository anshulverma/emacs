;;; av-setup.el --- Where all the magic begins  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'av-util)
(require 'f)
(require 'validate)

;; use-package is loaded early so setup files can safely wrap their
;; configuration in (use-package FOO :defer t :config ...) without
;; having to require it themselves. Deferral is opt-in per block —
;; making it the default breaks :config blocks that do work the
;; package is expected to have loaded (e.g. validate-setq).
(require 'use-package)
(setq use-package-expand-minimally t)

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

(if (file-exists-p "~/.emacs.local.el")
    (load "~/.emacs.local.el"))

;; load all files in custom folder while maintaining the order
(mapc 'load
      (f--files
       (expand-file-name "custom" av-src-dir)
       (and (s-matches? "^.*\.el$" it)
            (not (s-starts-with? "flycheck_" (f-filename it)))) t))

(provide 'av-setup)

;;; av-setup.el ends here
