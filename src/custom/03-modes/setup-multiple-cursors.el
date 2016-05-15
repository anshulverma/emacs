;;; setup-multiple-cursors --- Summary
;;; Commentary:
;;; Code:

(require 'multiple-cursors)

;; the following will add a cursor to each line of a active region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; add multiple cursors not based on continuous lines,
;; but based on keywords in the buffer --
;; (first mark the word, then add more cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; add cursor on click
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(provide 'setup-multiple-cursors)
;;; setup-multiple-cursors.el ends here
