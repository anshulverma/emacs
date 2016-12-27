;;; flyspell --- Summary
;;; Commentary:
;;; Code:

; turn on flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; use popup for flyspell correction
(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort (car (cdr (cdr poss))) 'string<)
                     (car (cdr (cdr poss)))))
         (cor-menu (if (consp corrects)
                       (mapcar (lambda (correct)
                                 (list correct correct))
                               corrects)
                     '()))
         (affix (car (cdr (cdr (cdr poss)))))
         show-affix-info
         (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                     (list
                                      (list (concat "Save affix: " (car affix))
                                            'save)
                                      '("Accept (session)" session)
                                      '("Accept (buffer)" buffer))
                                   '(("Save word" save)
                                     ("Accept (session)" session)
                                     ("Accept (buffer)" buffer)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu (mapcar
                (lambda (arg) (if (consp arg) (car arg) arg))
                base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
(eval-after-load "flyspell"
  '(progn
     (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

; use textual popup in terminal without window system
(defun flyspell-emacs-popup-choose (org-fun event poss word)
  (if (window-system)
      (funcall org-fun event poss word)
    (flyspell-emacs-popup-textual event poss word)))
(eval-after-load "flyspell"
  '(progn
     (defadvice flyspell-emacs-popup (around flyspell-emacs-popup-choose-advice)
       (flyspell-emacs-popup-choose))))

;; key bindings

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

(setq ispell-personal-dictionary (expand-file-name ".ispell" av-basedir))
(setq ispell-silently-savep t)

(add-hook 'flyspell-incorrect-hook
          (lambda (beg end sym)
            (message "%s misspelled. Type %s to fix it."
                     (buffer-substring beg end)
                     (substitute-command-keys "\\[endless/ispell-word-then-abbrev]"))
            ;; return nil so word is still highlighted.
            nil))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [s-mouse-1] #'flyspell-correct-word)))

(defun flyspell-check-next-highlighted-word ()
  "Move to next error and check it."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defhydra hydra-spell (:color red)
  "spell"
  ("s" flyspell-check-previous-highlighted-word "previous")
  ("n" flyspell-check-next-highlighted-word "next")
  ("c" ispell-continue "cont")
  ("e" flyspell-goto-next-error "next error")
  ("w" ispell-word "word")
  ("b" ispell-buffer "buffer")
  ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c f h") 'hydra-spell/body)

;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'.  Then create an abbrev for the correction made.
With prefix P, create local abbrev.  Otherwise it will be global."
  (interactive "P")
  (flyspell-goto-previous-error 1)
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(provide '03-flyspell)
;;; 03-flyspell.el ends here
