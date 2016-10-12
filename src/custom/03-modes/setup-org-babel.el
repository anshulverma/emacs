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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (sh . t)
   (ditaa . t)
   (dot . t)
   (plantuml . t)))

(provide 'setup-org-babel)
;;; setup-org-babel.el ends here
