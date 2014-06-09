;; ispell is hard to find in emacs
(if (eq system-type 'darwin)
    (setq ispell-program-name "/usr/local/bin/ispell")
    )
