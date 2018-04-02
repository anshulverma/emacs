;;; mode-features --- Org mode key bindings
;;; Commentary:
;;; Code:

;; Reference -- http://doc.norang.ca/org-mode.html

;; follow links if RET is pressed on them
(setq org-return-follows-link t)

;; automatically change list bullets
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

;; logging stuff
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; TODO keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(provide 'mode-features)
;;; mode-features.el ends here
