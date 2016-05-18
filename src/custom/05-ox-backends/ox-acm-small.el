;;; ox-acm-small -- Small standard format for ACM journal

;;; Commentary:
;; Package for exporting a journal in ACM's small standard format.
;; This was heavily inspired by John Kitchin's jmax.

;;; Code:

;;; Define Back-End
(require 'ox)

(add-to-list 'org-latex-classes
             '("acmsmall"                          ;class-name
               "\\documentclass{acmsmall}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"                                           ; header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(org-export-define-derived-backend 'av-acm-small 'latex
  :options-alist
  '((:author "AUTHOR" nil user-full-name newline)
    (:keyword "KEYWORD" nil nil newline)
    (:term "TERM" nil nil newline)
    (:acknowledgements "ACKNOWLEDGEMENTS" nil nil)
    (:abstract "ABSTRACT" nil "")
    (:bottomstuff "BOTTOMSTUFF" nil "")
    (:ccsdesc "CCSDESC" nil nil newline))
  :translate-alist '((template . av-acm-small-template))
  :menu-entry
  '(?a "ACM small standard format"
       ((?L "As LaTeX buffer" av-acm-small-export-as-latex)
        (?l "As LaTeX file" av-acm-small-export-to-latex)
        (?p "As PDF file" av-acm-small-export-to-pdf)
        (?o "As PDF file and open"
            (lambda (a s v b)
              (if a (av-acm-small-export-to-pdf t s v b)
                (org-open-file (av-acm-small-export-to-pdf nil s v b))))))))

(defun av-split-single-author (author)
  "Splits a AUTHOR into name and (optional) affiliation."
  (let ((parts (s-split-up-to "|" author 1 t)))
    (if (> 2 (length parts))
        (format "%s\n\\\\"(s-upcase author))
      (format "%s\n\\affil{%s}" (s-upcase (pop parts)) (pop parts)))))

(defun av-split-authors (authors)
  "Split all AUTHORS by comma (,)."
  (s-join "\n" (-map 'av-split-single-author (s-split "\n" authors t))))

(defun av-author-csv (authors)
  "Convert list of AUTHORS to a csv of names."
  (let* ((author-list
          (mapcar (function (lambda (author) (car (s-split-up-to "|" author 1 t))))
                  (s-split "\n" authors t))))
    (if (< 1 (length author-list))
        (format "%s and %s"
                (mapconcat 'identity (butlast author-list) ", ")
                (nth 0 (last author-list)))
      (nth 0 author-list))))

(defun av-acm-small-template (contents info)
  "Return complete document string by parsing CONTENTS and INFO."
  ;; (f-read-text "/Users/ansverma/Downloads/acmsmall/sample1.tex" 'utf-8)
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))

   ;; Document class and packages
   (let* ((class (plist-get info :latex-class))
          (class-options (plist-get info :latex-class-options))
          (header (nth 1 (assoc class org-latex-classes)))
          (document-class-string
           (and (stringp header)
                (if (not class-options) header
                  (replace-regexp-in-string
                   "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
                   class-options header t nil 1)))))
     (if (not document-class-string)
         (user-error "Unknown LaTeX class `%s'" class)
       (org-latex-guess-babel-language
        (org-latex-guess-inputenc
         (org-element-normalize-string
          (org-splice-latex-header
           document-class-string
           org-latex-default-packages-alist ; Defined in org.el.
           org-latex-packages-alist nil     ; Defined in org.el.
           (concat (org-element-normalize-string (plist-get info :latex-header))
                   (plist-get info :latex-header-extra)))))
        info)))

   "
% Package to generate and customize Algorithm as per ACM style
\\usepackage[ruled]{algorithm2e}
\\renewcommand{\\algorithmcfname}{ALGORITHM}
\\SetAlFnt{\\small}
\\SetAlCapFnt{\\small}
\\SetAlCapNameFnt{\\small}
\\SetAlCapHSkip{0pt}
\\IncMargin{-\\parindent}
"

   ;; Now the core content
   (let* ((signature-page (plist-get info :signature-page))
          (acknowledgements (plist-get info :acknowledgements))
          (abstract (plist-get info :abstract))
          (authors-list (nth 0 (plist-get info :author)))
          (authors (av-split-authors authors-list))
          (title (nth 0 (plist-get info :title)))
          (keywords (s-replace-all '(("\n" . ", ")) (plist-get info :keyword)))
          (terms (s-replace-all '(("\n" . ", ")) (plist-get info :term)))
          (bottomstuff (plist-get info :bottomstuff)))
     ;; org-mode escapes these in the abstract. This is hackery to
     ;; undo it. It is probably not fail-proof
     (setq abstract (org-export-data abstract info))
     (setq abstract (replace-regexp-in-string "\\\\\\$" "$" abstract))
     (setq abstract (replace-regexp-in-string "\\\\{" "{" abstract))
     (setq abstract (replace-regexp-in-string "\\\\}" "}" abstract))
     (setq abstract (replace-regexp-in-string "\\\\_" "_" abstract))
     (setq abstract (replace-regexp-in-string "\\$\\\\backslash\\$" "\\\\" abstract))
     (concat
      "
\\begin{document}

"
      (format "\\title{%s}" title)

      (format "\\author{%s}" authors)

      (format "\\begin{abstract}\n%s\n\\end{abstract}" abstract)

      "
\\ccsdesc[500]{Computer systems organization~Embedded systems}
\\ccsdesc[300]{Computer systems organization~Redundancy}
\\ccsdesc{Computer systems organization~Robotics}
\\ccsdesc[100]{Networks~Network reliability}
\n
"

      (format "\\terms{%s}" terms)

      (format "\\keywords{%s}" keywords)

      (format "\\acmformat{%s, %s. %s.}"
              (av-author-csv authors-list)
              (and (plist-get (nth 1 (nth 0 (plist-get info :date))) :year-start)
                   (format-time-string "%Y"))
              title)

      (format "\\begin{bottomstuff}\n%s\n\\end{bottomstuff}" bottomstuff)

      "
\\maketitle
\n\n"

      contents

      "
\n\\end{document}
"
      ))


   ))

;;;###autoload
(defun av-acm-small-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer in ACM small standard letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write content.

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org ACM Small Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (av-acm-small-special-contents)
    (org-export-to-buffer 'av-acm-small "*Org ACM Small Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun av-acm-small-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to  (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
        (av-acm-small-special-contents))
    (org-export-to-file 'av-acm-small outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun av-acm-small-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer in the ACM small standard format (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep))
        (av-acm-small-special-contents))
    (org-export-to-file 'av-acm-small file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun av-acm-small-export-to-pdf-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-open-file (av-acm-small-export-to-pdf async subtreep visible-only body-only ext-plist)))

(provide 'ox-acm-small)
;;; ox-acm-small ends here
