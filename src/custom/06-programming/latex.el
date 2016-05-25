;;; latex --- Summary
;;; Commentary:
;;; Code:

;; IMPORTANT -- make sure AUCTeX is installed:
;; brew install homebrew/tex/auctex

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq LaTeX-amsmath-label "eq:")
(setq LaTeX-command-style (quote (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))
(setq LaTeX-label-function (quote reftex-label))
(setq TeX-PDF-mode t)
(setq TeX-command-list
      (quote (("LaTeX"
               "%`%l%(mode)%' %t"
               TeX-run-TeX nil (latex-mode doctex-mode)
               :help "Run LaTeX")
              ("AmSTeX"
               "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t"
               TeX-run-TeX nil (ams-tex-mode)
               :help "Run AMSTeX")
              ("ConTeXt"
               "texexec --once --texutil %(execopts)%t"
               TeX-run-TeX nil (context-mode)
               :help "Run ConTeXt once")
              ("ConTeXt Full"
               "texexec %(execopts)%t"
               TeX-run-TeX nil (context-mode)
               :help "Run ConTeXt until completion")
              ("BibTeX"
               "bibtex %s"
               TeX-run-BibTeX nil t
               :help "Run BibTeX")
              ("Biber"
               "biber %s"
               TeX-run-Biber nil t
               :help "Run Biber")
              ("View"
               "%V"
               TeX-run-discard-or-function t t
               :help "Run Viewer")
              ("Print"
               "%p"
               TeX-run-command t t
               :help "Print the file")
              ("Queue"
               "%q"
               TeX-run-background nil t
               :help "View the printer queue"
               :visible TeX-queue-command)
              ("File"
               "%(o?)dvips %d -o %f "
               TeX-run-command t t
               :help "Generate PostScript file")
              ("Index"
               "makeindex %s"
               TeX-run-command nil t
               :help "Create index file")
              ("Check"
               "lacheck %s"
               TeX-run-compile nil (latex-mode)
               :help "Check LaTeX file for correctness")
              ("Spell"
               "(TeX-ispell-document \"\")"
               TeX-run-function nil t
               :help "Spell-check the document")
              ("Clean"
               "TeX-clean"
               TeX-run-function nil t
               :help "Delete generated intermediate files")
              ("Clean All"
               "(TeX-clean t)"
               TeX-run-function nil t
               :help "Delete generated intermediate and output files")
              ("Other"
               ""
               TeX-run-command t t
               :help "Run an arbitrary command"))))
(setq TeX-electric-sub-and-superscript t)
(setq TeX-source-correlate-method (quote synctex))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server nil)
(setq TeX-view-program-list (quote (("OS X Preview" "open %o"))))
(setq TeX-view-program-selection
      (quote ((output-pdf "OS X Preview"))))
(setq preview-TeX-style-dir ; this may change
      "~/.emacs.d/.cask/24.5.1/elpa/auctex-11.89.3/latex")
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'company-mode)
;; make sure to install Zotero "brew install Caskroom/cask/zotero"
(add-hook 'LaTeX-mode-hook 'zotelo-minor-mode)

(setq reftex-label-alist (quote (("thm" nil nil nil nil nil -3))))
(setq reftex-plug-into-AUCTeX t)

(require 'company-auctex)

(eval-after-load 'info-look '(let ()
                                (info-lookup-add-help
                                 :mode 'latex-mode
                                 :regexp ".*"
                                 :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                                 :doc-spec '(("(latex2e)Concept Index" )
                                             ("(latex2e)Command Index")))
                                ))

;; automagic detection of master file
(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))

(add-hook 'LaTeX-mode-hook
          '(setq TeX-master (guess-TeX-master (buffer-file-name))))

;; highlight (or font-lock) the “\section{title}” lines:
(font-lock-add-keywords
 'latex-mode
 `((,(concat "^\\s-*\\\\\\("
             "\\(documentclass\\|\\(sub\\)?section[*]?\\)"
             "\\(\\[[^]% \t\n]*\\]\\)?{[-[:alnum:]_ ]+"
             "\\|"
             "\\(begin\\|end\\){document"
             "\\)}.*\n?")
    (0 'your-face append))))

(provide 'latex)
;;; latex.el ends here
