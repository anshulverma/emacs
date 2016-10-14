;;; org --- Summary
;;; Commentary:
;;; Code:

(setq org-latex-create-formula-image-program 'dvipng)

(require 'jmax-org)

;; enabling flyspell in special edit source blocks.
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name))) ;; (1)
    ad-do-it				  ;; (2)
    (setq buffer-file-name file-name))) ;; (3)

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; configure ispell to ignore some things
(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  ;; this next line approximately ignores org-ref-links
  (add-to-list 'ispell-skip-region-alist '("cite:" . " "))
  (add-to-list 'ispell-skip-region-alist '("label:" . " "))
  (add-to-list 'ispell-skip-region-alist '("ref:" . " "))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'endless/org-ispell)

(setq org-src-fontify-natively t)

;; set up agenda so it reads from dropbox
(setq org-agenda-files '("~/Dropbox/org"))

;; change color based on colander type
(add-hook 'org-finalize-agenda-hook
          (lambda ()
            (save-excursion
              (color-org-header "Personal:"  "#97c71c")
              (color-org-header "Birthday:"  "#ff8000")
              (color-org-header "Work:"      "#fc5f5f")
              (color-org-header "Holiday:"   "#008282"))))

(defun color-org-header (tag col)
  "Change color of a agenda item based on TAG to COL."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward tag nil t)
    (add-text-properties (match-beginning 0) (point-at-eol)
                         `(face (:foreground ,col)))))

(provide '05-org)
;;; 05-org.el ends here
