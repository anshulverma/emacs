(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") ; duplicate current line

;; easy spell check
(global-set-key (kbd "C-c f i") 'ispell-word)
(global-set-key (kbd "C-c f f") 'flyspell-mode)
(global-set-key (kbd "C-c f b") 'flyspell-buffer)
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))))))
(defun flyspell-check-previous-misspelled-word ()
  "Custom function to spell check previous misspelled word"
  (interactive)
  (flyspell-goto-previous-error 1)
  (flyspell-correct-word-before-point))
(global-set-key (kbd "C-c f p") 'flyspell-check-previous-misspelled-word)
(defun flyspell-check-next-misspelled-word ()
  "Custom function to spell check next misspelled word"
  (interactive)
  (flyspell-goto-next-error)
  (flyspell-correct-word-before-point))
(global-set-key (kbd "C-c f n") 'flyspell-check-next-misspelled-word)
(global-set-key (kbd "C-c f c") 'flyspell-correct-word-before-point)

;;; whitespace
(global-set-key (kbd "\C-c w") 'whitespace-mode)
(global-set-key (kbd "\C-c t") 'whitespace-toggle-options)
(global-set-key (kbd "\C-c W") 'global-whitespace-mode)
(global-set-key (kbd "\C-c T") 'global-whitespace-toggle-options)
