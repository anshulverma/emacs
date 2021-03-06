;;; calendar --- Summary
;;; Commentary:
;;; Code:

;; -------- calfw setup --------

(require 'calfw)
(require 'calfw-org)
(require 'calfw-cal)
(require 'f)
(require 's)

(defun av/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "#44aa77")  ; orgmode source
    (cfw:cal-create-source "#aa6677")  ; diary source
    )))

(setq cfw:org-overwrite-default-keybinding t)
(setq cfw:org-agenda-schedule-args '(:scheduled :sexp :closed :deadline :todo :timestamp))
(setq org-agenda-show-log-scoped t) ;; hack to fix org-agenda error

;; Month
(setq calendar-month-name-array
      ["January" "February" "March"     "April"   "May"      "June"
       "July"    "August"   "September" "October" "November" "December"])

;; Week days
(setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;; First day of the week
(setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

(add-hook 'cfw:calendar-mode-hook
          (lambda()
            (setq show-trailing-whitespace nil)))
(add-hook 'calendar-mode-hook
          (lambda()
            (setq show-trailing-whitespace nil)))

;; -----------------------------

(require 'calendar)

(setq calendar-view-diary-initially-flag t)
(setq org-agenda-include-diary t)
(setq calendar-mark-diary-entries-flag t)
(setq diary-entry-marker 'font-lock-constant-face)

(setq calendar-time-zone -300)
(setq calendar-standard-time-zone-name "PST")
(setq calendar-daylight-time-zone-name "PDT")

(setq calendar-latitude [37 46 north])
(setq calendar-longitude [122 25 west])

(setq calendar-location-name "San Francisco, CA")
(setq calendar-time-display-form '(12-hours ":" minutes))

;; mark today as underlined
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(if (and (boundp 'av/org-base-dir)
         (f-exists? av/org-base-dir))
    (progn
      (message (concat "Using '" av/org-base-dir "' for setting up org-agenda."))
      (setq diary-file (f-join av/org-base-dir "diary"))
      (setq org-agenda-files
            (f-files av/org-base-dir
                     (lambda (file)
                       (s-matches? "^[a-zA-Z0-9\-]*\.org$" (f-filename file))))))
  (message "Not setting up org agenda files because 'av/org-base-dir' is not set."))

;;; number of diary entries to display, from Sunday to Saturday
(setq number-of-diary-entries [2 1 1 1 1 4 2])

;; Holidays
(setq islamic-holidays 'nil)
(setq hebrew-holidays 'nil)
(setq oriental-holidays 'nil)

(setq calendar-mark-holidays-flag t)
(setq calendar-view-holidays-initially t)
(setq calendar-mark-diary-entries-flag t)
(setq diary-show-holidays-flag nil)

;; Appointments
(setq appt-message-warning-time 10)
(setq appt-audible t)
(setq appt-visible t)
(setq appt-display-mode-line t)
(setq appt-display-duration 10)
(setq appt-display-interval 2)

;; set to window to allow growl to notify
(setq appt-display-format 'window)

;; set to 1 to activate the appointment function
(appt-activate 1)

;; redefine some keys
(define-key calendar-mode-map "\M-n" 'calendar-forward-month) ; was ESC-}
(define-key calendar-mode-map "\M-p" 'calendar-backward-month) ; was ESC-{
(define-key calendar-mode-map "\C-\M-n" 'calendar-forward-year) ; was C-x [
(define-key calendar-mode-map "\C-\M-p" 'calendar-backward-year) ; was C-x [

(provide '06-calendar)
;;; 06-calendar.el ends here
