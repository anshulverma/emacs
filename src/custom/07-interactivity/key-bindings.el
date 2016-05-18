;;; key-bindings --- Custom interactive functions and their key bindings.

;;; Commentary:

;;; Code:

;; key chords
(require 'key-chord)

(key-chord-define-global "BB" 'switch-to-previous-buffer)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "JB" 'beginning-of-buffer)
(key-chord-define-global "JE" 'end-of-buffer)
(key-chord-mode +1)

(global-set-key (kbd "C-c d") 'duplicate-current-line)

;; join lines
(global-set-key (kbd "C-c j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "<C-M-up>") 'move-line-up)
(global-set-key (kbd "<C-M-down>") 'move-line-down)

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key (kbd "C-c t") 'hydra-av-togglers/body)

(global-set-key (kbd "<f5>") 'compile-again)

(global-set-key (kbd "<backtab>") 'un-indent)

(provide 'key-bindings)
;;; key-bindings.el ends here
