;;; setup-org-babel --- Summary
;;; Commentary:
;;; Code:

(require 'ox-html)
(require 'ob-ditaa)
(require 'ob-plantuml)
(require 'ob-ipython)

(setq org-html-htmlize-output-type 'css)
(setq org-ditaa-jar-path (av/brew-file "ditaa" "jar"))
(setq org-plantuml-jar-path (av/brew-file "plantuml" "jar"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sh . t)
   (matlab . t)
   (sqlite . t)
   (ruby . t)
   (perl . t)
   (org . t)
   (dot . t)
   (plantuml . t)
   (R . t)
   (C . t)
   (clojure . t)
   (sh . t)
   (ditaa . t)
   (dot . t)
   (gnuplot . t)
   (ipython . t)
   (http . t)
   (restclient . t)))

(provide 'setup-org-babel)
;;; setup-org-babel.el ends here
