;;; python --- Python support  -*- lexical-binding: t; -*-

;;; Commentary:

;; Minimal modern Python stack.  The built-in python.el has been very
;; capable for years, so we don't load python-mode.el any more.  For
;; completion/diagnostics we use eglot (built-in on Emacs 29+) with
;; pyright or ruff-lsp as the server, and apheleia for format-on-save
;; via black or ruff.
;;
;; Servers and formatters are probed at runtime, so the config
;; degrades gracefully if the user hasn't installed them.
;;
;; Pre-requisites (install with pipx or pip --user):
;;   pipx install pyright          # type checker / completion
;;   pipx install ruff             # fast linter + formatter
;;   pipx install black            # alternative formatter

;;; Code:

(require 'python)

(setq python-indent-guess-indent-offset-verbose nil)

;; Treat pyproject.toml / setup.cfg as python project roots so eglot
;; finds the right directory automatically.
(with-eval-after-load 'project
  (dolist (marker '("pyproject.toml" "setup.cfg" "setup.py" "requirements.txt"))
    (add-to-list 'project-vc-extra-root-markers marker)))

(defun av/python-eglot-maybe ()
  "Start eglot for this buffer if a supported LSP server is on PATH."
  (when (and (fboundp 'eglot-ensure)
             (or (executable-find "pyright")
                 (executable-find "pyright-langserver")
                 (executable-find "ruff")
                 (executable-find "basedpyright")))
    (eglot-ensure)))

(add-hook 'python-mode-hook #'av/python-eglot-maybe)
(when (fboundp 'python-ts-mode)
  (add-hook 'python-ts-mode-hook #'av/python-eglot-maybe))

(use-package apheleia
  :ensure t
  :hook ((python-mode python-ts-mode) . apheleia-mode)
  :config
  ;; Prefer ruff (fastest) and fall back to black.
  (setf (alist-get 'python-mode apheleia-mode-alist)
        (cond ((executable-find "ruff")  'ruff-format)
              ((executable-find "black") 'black)
              (t nil)))
  (when (boundp 'python-ts-mode)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist)
          (alist-get 'python-mode apheleia-mode-alist))))

(provide 'python)
;;; python.el ends here
