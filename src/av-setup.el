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

;; GUI Emacs on macOS doesn't inherit PATH from the user's shell;
;; exec-path-from-shell runs the shell to capture it. Use a login
;; shell (-l) but NOT interactive — interactive shells source
;; .zshrc / .bashrc, whose prompts and greetings pollute the output
;; and crash exec-path-from-shell's printf framing. Wrap the whole
;; block in with-demoted-errors so a single shell misconfig never
;; aborts init.
(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-check-startup-files nil)
  (with-demoted-errors "exec-path-from-shell: %S"
    (exec-path-from-shell-initialize)
    ;; Copy only things that are likely to exist and that Emacs cares
    ;; about. rvm-related vars are only needed if the user actually
    ;; uses rvm; exec-path-from-shell-copy-env silently no-ops when
    ;; the var isn't set.
    (dolist (var '("GEM_HOME" "GEM_PATH" "MY_RUBY_HOME" "RUBY_VERSION"
                   "rvm_env_string" "rvm_ruby_string"))
      (exec-path-from-shell-copy-env var))))

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
