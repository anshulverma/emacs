;;; ----MAC OSX----
(if (eq system-type 'darwin)

    ;; ispell is hard to find in emacs
    (setq ispell-program-name "/usr/local/bin/ispell")

    ;; make COMMAND key function as CTRL
    (setq mac-command-modifier 'control))
