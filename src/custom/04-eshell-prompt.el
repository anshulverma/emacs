;;; eshell-prompt --- Summary
;;; Commentary:
;;; Code:

(setq eshell-history-size (* 1024 10))
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-output (shell-command-to-string
                        (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'")))
           (git-status (shell-command-to-string (concat "cd " pwd " && git status --short")))
           (git-branch-color (if (s-blank? git-status) "#77ff77" "#ff7777")))
      (propertize (concat "("
                          (if (> (length git-output) 0)
                              (substring git-output 0 -1)
                            "no branch")
                          ") ") 'face `(:foreground ,git-branch-color))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize ((lambda (p-lst)
                        (if (> (length p-lst) 3)
                            (concat
                             (mapconcat (lambda (elm) (if (< (length elm) 2) elm
                                                        (substring elm 0 2)))
                                        (butlast p-lst 3)
                                        "/")
                             "/"
                             (mapconcat (lambda (elm) elm)
                                        (last p-lst 3)
                                        "/"))
                          (mapconcat (lambda (elm) elm)
                                     p-lst
                                     "/")))
                      (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "#ffff77"))
         (propertize " # " 'face 'default))))

(setq eshell-highlight-prompt nil)

(add-hook 'eshell-mode-hook
          (lambda()
            (setq show-trailing-whitespace nil)))

(provide '04-eshell-prompt)
;;; 04-eshell-prompt.el ends here
