;;; setup-org-babel --- Summary
;;; Commentary:
;;; Code:

(setq org-html-htmlize-output-type 'css)

(defun av/org-babel-get-lib-jar (path)
  (let
      ((lib-files (f-files path (lambda (file) (equal (f-ext file) "jar")))))
    (if (equal 1 (length lib-files))
        (car lib-files)
      (error (s-concat "Invalid number of jar files in " path)))))

(setq org-ditaa-jar-path
      (av/org-babel-get-lib-jar
       (f-join (s-trim (shell-command-to-string "brew --prefix ditaa")) "libexec")))

(setq org-plantuml-jar-path
      (av/org-babel-get-lib-jar
       (s-trim (shell-command-to-string "brew --prefix plantuml"))))

(require 'ob-ipython)

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
