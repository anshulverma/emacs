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

;; no need to prompt for confirmation of code block evaluation in case of dot
(defun av/org-confirm-babel-evaluate (lang body)
  "Don't prompt for bot blocks."
  (not (string= lang "dot")))
(setq org-confirm-babel-evaluate 'av/org-confirm-babel-evaluate)

;; display inline images when evaluating blocks
(define-key org-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (org-ctrl-c-ctrl-c)
    (org-display-inline-images)))

(provide '05-org)
;;; 05-org.el ends here
