;;; capture-templates --- Org mode capture templates
;;; Commentary:
;;; Code:

;; Reference  -- http://doc.norang.ca/org-mode.html

(require 'org-protocol)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (f-join org-directory "notes.org"))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (f-join org-directory "journal.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-default-notes-file)
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-default-notes-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(provide 'capture-templates)
;;; capture-templates.el ends here
