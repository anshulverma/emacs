;;; python --- Summary
;;; Commentary:
;;; Code:

(require 'python-mode)

(elpy-enable)
(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; ;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; ;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; ;; don't split windows
(setq py-split-windows-on-execute-p nil)

;; ;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; no whitespace in inferior python mode
(add-hook 'inferior-python-mode-hook
          (lambda()
            (setq show-trailing-whitespace nil)))

;;---------EIN---------

(require 'ein)

;; enable autocomplete in ein
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(setq ein:use-auto-complete t)

;;---------------------

(provide 'python)
;;; python.el ends here
