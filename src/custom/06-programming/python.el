;;; python --- Summary
;;; Commentary:
;;; Code:

(require 'python-mode)

;;--------PyENV--------

;; set the right version of python using pyenv
(require 'pyenv-mode)

(defvar av/pyenv-default-version "2.7.10"
  "Default version of python to use in case .python-version file is absent.")

(defvar av/pyenv-basedir "/usr/local/var/pyenv"
  "Base directory where pyenv is located.")

(defun av/pyenv-mode-auto-hook ()
  "Automatically activates pyenv version if .python-version file exists."
  (pyenv-mode 1)
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8)))
         (pyenv-mode-set av/pyenv-default-version))))))

(add-hook 'python-mode-hook 'av/pyenv-mode-auto-hook)

(define-key pyenv-mode-map (kbd "C-c C-s") nil)
(define-key pyenv-mode-map (kbd "C-c C-u") nil)

(add-to-list 'exec-path (f-join av/pyenv-basedir "shims"))

;;---------------------

(elpy-enable)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Use Jedi instead of buggy Rope
(setq elpy-rpc-backend "jedi")
(setq python-check-command (expand-file-name (f-join av/pyenv-basedir "shims/flake8")))
(setq python-check-command "flake8")

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; don't split windows
(setq py-split-windows-on-execute-p nil)

;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; no whitespace in inferior python mode
(add-hook 'inferior-python-mode-hook
          (lambda()
            (setq show-trailing-whitespace nil)))

;; set up jedi
(defun av/python-mode-hook ()
  "Setup autocomplete."
  (add-to-list 'company-backends 'company-jedi)
  (jedi:setup)
  (subword-mode 1))

(setq jedi:complete-on-dot t)

;;---------EIN---------

(require 'ein)

;; enable autocomplete in ein
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(setq ein:use-auto-complete t)

;;---------------------

;; org babel async execution for python -- useful for long running tasks
(defun org-babel-async-execute:python ()
  "Execute the python src-block at point asynchronously.
:var headers are supported.
:results output is all that is supported for output.

A new window will pop up showing you the output as it appears,
and the output in that window will be put in the RESULTS section
of the code block."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (uuid (org-id-uuid))
         (code (org-element-property :value (org-element-context)))
         (temporary-file-directory ".")
         (tempfile (make-temp-file "py-"))
         (pbuffer (format "*%s*" uuid))
         (varcmds (org-babel-variable-assignments:python
                   (nth 2 (org-babel-get-src-block-info))))
         process)

    ;; get rid of old results, and put a place-holder for the new results to
    ;; come.
    (org-babel-remove-result)

    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (insert (format
               "\n\n#+RESULTS: %s\n: %s"
               (or (org-element-property :name (org-element-context))
                   "")
               uuid)))

    ;; open the results buffer to see the results in.
    (switch-to-buffer-other-window pbuffer)

    ;; Create temp file containing the code.
    (with-temp-file tempfile
      ;; if there are :var headers insert them.
      (dolist (cmd varcmds)
        (insert cmd)
        (insert "\n"))
      (insert code))

    ;; run the code
    (setq process (start-process
                   uuid
                   pbuffer
                   "python"
                   tempfile))

    ;; when the process is done, run this code to put the results in the
    ;; org-mode buffer.
    (set-process-sentinel
     process
     `(lambda (process event)
        (save-window-excursion
          (save-excursion
            (save-restriction
              (with-current-buffer (find-file-noselect ,current-file)
                (goto-char (point-min))
                (re-search-forward ,uuid)
                (beginning-of-line)
                (kill-line)
                (insert
                 (mapconcat
                  (lambda (x)
                    (format ": %s" x))
                  (butlast (split-string
                            (with-current-buffer
                                ,pbuffer
                              (buffer-string))
                            "\n"))
                  "\n"))))))
        ;; delete the results buffer then delete the tempfile.
        ;; finally, delete the process.
        (when (get-buffer ,pbuffer)
          (kill-buffer ,pbuffer)
          (delete-window))
        (delete-file ,tempfile)
        (delete-process process)))))

(provide 'python)
;;; python.el ends here
