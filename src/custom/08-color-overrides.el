;;; 08-color-overrides --- Custom interactive functions.

;;; Commentary:

;;; Code:

(add-hook 'av/theme-load-hook 'av/switch-cfw-color-by-theme)

(defun av/switch-cfw-color-by-theme (theme)
  "Switch the color for cfw calendar based on THEME."
  (if (eq theme "zenburn")
      (progn
        (require 'zenburn-theme)
        (custom-theme-set-faces
         'zenburn
         '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
         '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
         '(cfw:face-sunday ((t :foreground "#787878")))
         '(cfw:face-saturday ((t :foreground "#787878")))
         '(cfw:face-holiday ((t :foreground "#78aa78" :inherit cfw:face-day-title)))
         '(cfw:face-grid ((t :foreground "#8f8f8f" :weight bold)))
         '(cfw:face-default-content ((t :foreground "#bfebbf")))
         '(cfw:face-periods ((t :foreground "cyan")))
         '(cfw:face-day-title ((t :background "#333333")))
         '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
         '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
         '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
         '(cfw:face-today ((t :background "#334455" :weight bold)))
         '(cfw:face-today-title ((t :background "#335566" :weight bold)))
         '(cfw:face-select ((t :background "#aaaaaa")))
         '(cfw:face-toolbar ((t :foreground "Steelblue4")))
         '(cfw:face-toolbar-button-off ((t :foreground "#656565" :weight bold)))
         '(cfw:face-toolbar-button-on ((t :foreground "#bbbbbb" :weight bold)))))))


(provide 'color-overrides)
;;; 08-color-overrides.el ends here
