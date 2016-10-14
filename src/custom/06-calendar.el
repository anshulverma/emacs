;;; calendar --- Summary
;;; Commentary:
;;; Code:

(require 'calendar)

(setq calendar-view-diary-initially-flag t)
(setq calendar-mark-diary-entries-flag t)
(setq diary-entry-marker 'font-lock-reference-face)

(setq calendar-time-zone -300)
(setq calendar-standard-time-zone-name "PST")
(setq calendar-daylight-time-zone-name "PDT")

(setq calendar-latitude [37 46 north])
(setq calendar-longitude [122 25 west])

(setq calendar-location-name "San Francisco, CA")
(setq calendar-time-display-form '(12-hours ":" minutes))

;; mark today as underlined
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; Diary
(setq diary-file (concat "~/Dropbox/org/diary"))

;;; number of diary entries to display, from Sunday to Saturday
(setq number-of-diary-entries [2 1 1 1 1 4 2])

;; Holidays
(setq islamic-holidays 'nil)
(setq hebrew-holidays 'nil)
(setq oriental-holidays 'nil)

(setq diary-show-holidays-flag t)
(setq calendar-mark-holidays-flag t)
(setq calendar-view-holidays-initially t)
(setq calendar-mark-diary-entries-flag t)

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
