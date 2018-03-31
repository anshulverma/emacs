;;; org --- Summary
;;; Commentary:
;;; Code:

(setq org-latex-create-formula-image-program 'dvipng)

;; Disabled because there are issues with loading it currently
;; described in -- https://lists.gnu.org/archive/html/emacs-orgmode/2016-02/msg00424.html
;; (require 'jmax-org)

(defadvice org-edit-src-code
    (around set-buffer-file-name activate compile)
  "Enabling flyspell in special edit source blocks."
  (let ((file-name (buffer-file-name))) ;; (1)
    ad-do-it                            ;; (2)
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

;; change color based on colander type
(add-hook 'org-finalize-agenda-hook
          (lambda ()
            (save-excursion
              (color-org-header "Personal:"  "#97c71c")
              (color-org-header "Diary:"     "#e855e4")
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

;; enable `org-bullets'
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; set up `org-page' for blog
(require 'org-page)

(if (and (boundp 'av/blog-dir)
         (f-exists? av/blog-dir))
    (progn
      (setq op/repository-directory av/blog-dir)
      (setq op/site-domain
            (if (boundp 'av/blog-site-domain)
                av/blog-site-domain
              nil))
      (setq op/personal-disqus-shortname
            (if (boundp 'av/blog-disqus-username)
                av/blog-disqus-username
              nil))
      (setq op/personal-google-analytics-id
            (if (boundp 'av/blog-google-analytics-id)
                av/blog-google-analytics-id
              nil))))

;; org mode exported via pandoc
(require 'ox-pandoc)

(provide '05-org)
;;; 05-org.el ends here
