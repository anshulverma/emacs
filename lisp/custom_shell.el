(if (equal (getenv "EMACS_SHELL") "1")
       (print "EMACS_SHELL is set; adding shell to your emacs session"
    ;; run shell in emacs
    (shell))
  (print "EMACS_SHELL is not set; skipping shell customization"))
