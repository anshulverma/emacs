;;; irc --- Summary
;;; Commentary:
;;; Code:

(require 'erc)

;;  new command "/uptime"
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

;; do not use whitespace mode in ERC
(setq whitespace-global-modes '(not erc-mode))

;; autojoin channels of interest
(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#gnustep" "#latex" "#screen"
         "#fnr" "#fnr-staff" "#ducttape" "#carvux" "#unit-e" "#isys"
         "#fsptb" "#freestream")
        ("crystalia.net" "#crystalia")))

;; match some keywords
(require 'erc-match)
(setq erc-keywords '("anshul" "verma"))
(erc-match-mode)

;; track active channels in modeline
(require 'erc-track)
(erc-track-mode t)

;; enable programmatic completion
(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

;; split messages with long lines into multiple lines
(require 'erc-fill)
(erc-fill-mode t)

;; use an input ring to maintain a history of stuff you wrote
(require 'erc-ring)
(erc-ring-mode t)

;; do not show too many quit messages
(require 'erc-netsplit)
(erc-netsplit-mode t)

;; enable timestamp mode
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

;; disable button mode as it is slow
(erc-button-mode nil)

;; set up username and email id
(defvar av/user-id "anshulverma" "ID for the user to be used for IRC.")
(defvar av/user-full-name "Anshul Verma" "Full name for the user to be used for IRC.")
(setq erc-user-full-name av/user-full-name)
(if (boundp 'av/user-email)
    (setq erc-email-userid av/user-email)
  (message "WARNING: 'av/user-email' is not set"))


;; logging:
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

;; save buffers when quitting emacs
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                        (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                  (set (make-variable-buffer-local
                                        'coding-system-for-write)
                                       'emacs-mule))))
;; end logging

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)

;;; Finally, connect to the networks.
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
         :nick av/user-id :full-name av/user-full-name)
    (erc :server "irc.crystalia.net" :port 6667
         :nick av/user-id :full-name av/user-full-name)))

;; channel specific prompt
(setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

(provide 'irc)
;;; irc.el ends here
