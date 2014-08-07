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

      ;; resize font height for mac
      (set-face-attribute 'default nil :height 154)

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

      ;; set function key as hyper
      (setq ns-function-modifier 'hyper)))
