;;; setup-claude-code-ide --- Claude Code CLI integration  -*- lexical-binding: t; -*-

;;; Commentary:

;; claude-code-ide.el bridges Emacs to the Claude Code CLI via MCP,
;; exposing Emacs' project/xref/tree-sitter context to Claude and
;; rendering ediff-style suggestions in-buffer.
;;
;; Prerequisites:
;;   - Emacs 29+ (uses the built-in `package-vc-install')
;;   - Claude Code CLI on PATH (`claude' executable)
;;   - `eat' terminal backend (installed via av/packages)
;;
;; This file no-ops cleanly when any prerequisite is missing, so a
;; fresh machine or a CI run without the CLI still loads init.el.

;;; Code:

(defun av/claude-code-ide--install ()
  "Fetch claude-code-ide.el via package-vc if it isn't already installed."
  (unless (package-installed-p 'claude-code-ide)
    (with-demoted-errors "claude-code-ide install failed: %S"
      (package-vc-install
       '(claude-code-ide
         :url "https://github.com/manzaltu/claude-code-ide.el")))))

(when (and (>= emacs-major-version 29)
           (executable-find "claude"))
  (av/claude-code-ide--install)

  (when (package-installed-p 'claude-code-ide)
    (use-package claude-code-ide
      :defer t
      :bind (("C-c c" . claude-code-ide-menu))
      :custom (claude-code-ide-terminal-backend 'eat)
      :config (claude-code-ide-emacs-tools-setup))))

(provide 'setup-claude-code-ide)

;;; setup-claude-code-ide.el ends here
