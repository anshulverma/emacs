;;; setup-gptel --- Claude chat via gptel  -*- lexical-binding: t; -*-

;;; Commentary:

;; gptel is a provider-agnostic LLM client.  Here we configure it so
;; Claude (Anthropic) is the default backend.
;;
;; API key resolution, in order:
;;   1. ANTHROPIC_API_KEY env var  (convenient on Codespaces via repo secrets)
;;   2. auth-source entry for api.anthropic.com  (e.g. ~/.authinfo / .authinfo.gpg)
;;
;; Example auth-source line:
;;   machine api.anthropic.com login apikey password sk-...
;;
;; Override the default model from ~/.emacs.local.el:
;;   (setq av/claude-model 'claude-opus-4-7)

;;; Code:

(defvar av/claude-model 'claude-sonnet-4-6
  "Default Claude model symbol for gptel.")

(defun av/gptel--anthropic-key ()
  "Return the Anthropic API key from env or auth-source, or nil."
  (or (getenv "ANTHROPIC_API_KEY")
      (auth-source-pick-first-password
       :host "api.anthropic.com"
       :user "apikey")))

(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-c a c" . gptel)
         ("C-c a s" . gptel-send)
         ("C-c a m" . gptel-menu))
  :config
  (setq gptel-default-mode 'org-mode
        gptel-model av/claude-model
        gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key #'av/gptel--anthropic-key
          :models '(claude-opus-4-7
                    claude-sonnet-4-6
                    claude-haiku-4-5-20251001))))

(provide 'setup-gptel)

;;; setup-gptel.el ends here
