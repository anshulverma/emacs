 ;;; scala --- Show help popups for prefix keys.

;;; Commentary:

;;; Code:

(use-package scala-mode                 ; Scala editing
  :ensure t
  :defer t
  :config
  (validate-setq scala-indent:default-run-on-strategy
                 scala-indent:operator-strategy)
  (defun lunaryorn-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (define-key scala-mode-map (kbd "RET")
    #'lunaryorn-newline-and-indent-with-asterisk))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :bind (:map scala-mode-map
              ("C-c m b c" . sbt-command)
              ("C-c m b r" . sbt-run-previous-command))
  :config
  ;; Do not pop up SBT buffers automatically
  (validate-setq sbt:display-command-buffer nil)
  (defun lunaryorn-scala-pop-to-sbt (new-frame)
    "Open SBT REPL for this project, optionally in a NEW-FRAME.

Select the SBT REPL for the current project in a new window.  If
the REPL is not yet running, start it.  With prefix arg, select
the REPL in a new frame instead."
    (interactive "P")
    ;; Start SBT when no running, taken from `sbt:command'
    (when (not (comint-check-proc (sbt:buffer-name)))
      (sbt:run-sbt))
    (let ((display-buffer-overriding-action
           (if new-frame '(display-buffer-pop-up-frame) nil)))
      (pop-to-buffer (sbt:buffer-name))))
  (with-eval-after-load 'scala-mode
    (bind-key "C-c m s" #'lunaryorn-scala-pop-to-sbt scala-mode-map))
  ;; Disable Smartparens Mode in SBT buffers, because it frequently
  ;; hangs while trying to find matching delimiters
  (add-hook 'sbt-mode-hook
            (lambda ()
              (when (fboundp 'smartparens-mode)
                (smartparens-mode -1))))

  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :pin melpa-stable
  :after scala-mode
  :bind (:map ensime-mode-map
              ("C-c m E" . ensime-reload)
              ;; Free M-n and M-p again
              ("M-n" . nil)
              ("M-p" . nil)
              ("<f5>" . ensime-sbt-do-compile)
              :map scala-mode-map ("C-c m e" . ensime))
  :config
  ;; ;; Enable Ensime for all Scala buffers.
  (add-hook 'scala-mode-hook #'ensime-mode)
  ;; Compile on save.  My projects are small enough :)
  (validate-setq ensime-sbt-perform-on-save "test:compile"))
(use-package ensime-expand-region       ; Integrate Ensime into expand-region
  :ensure ensime
  :after ensime)
(use-package play-routes-mode           ; Mode for Play 2 routes files
  :ensure t
  :defer t)
(use-package flycheck-ensime            ; Ensime-based checker for Flycheck
  :disabled t
  :load-path "lisp/"
  :defer t)

(provide 'scala)
;;; scala.el ends here
