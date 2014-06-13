(defgroup nuaavee nil
  "Custom support for nuaavee's emacs environment."
  :group 'emacs)

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
(global-set-key (kbd "\C-c t w") 'whitespace-toggle-options)
(global-set-key (kbd "\C-c W") 'global-whitespace-mode)
(global-set-key (kbd "\C-c t W") 'global-whitespace-toggle-options)

;;; copy lines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))
(global-set-key (kbd "\C-c\C-k") 'copy-line)

;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
          (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line)

;; shrink/enlarge buffers
(define-minor-mode buffer-resize-mode
  "Toggles buffer-resize-mode.
    This command toggles the buffer resize mode. When enabled, buffer resize mode allows you to use
    W, A, S, D keys to resize the current buffer."
  :init-value nil
  :lighter " BufferResize"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-w") 'enlarge-window)
            (define-key map (kbd "C-a") 'shrink-window-horizontally)
            (define-key map (kbd "C-s") 'shrink-window)
            (define-key map (kbd "C-d") 'enlarge-window-horizontally)
            map)
  :group 'nuaavee)
(global-set-key (kbd "C-c t r") 'buffer-resize-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; join lines
(global-set-key (kbd "C-c j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; magit status buffer
(global-set-key (kbd "C-c g") 'magit-status)

;; delete file
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

; rename current file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x <f2>") 'rename-current-buffer-file)

; move lines around
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(global-set-key (kbd "<C-M-up>") 'move-line-up)
(global-set-key (kbd "<C-M-down>") 'move-line-down)

; open line above or below current line
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

; kill shel easily with C-d twice
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

; temporary full-screen toggle
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(global-set-key (kbd "C-c t f") 'toggle-maximize-buffer)

; icicle bindings
(global-set-key (kbd "C-c i") 'imenu)

; compiling and re-compiling
(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
  (save-some-buffers 1)
  (interactive "p")
  (if (and (eq pfx 1)
           compilation-last-buffer)
      (progn
        (set-buffer compilation-last-buffer)
        (revert-buffer t t))
    (call-interactively 'compile)))
(global-set-key (kbd "<f5>") 'compile-again)

; regular expression builder
(global-set-key (kbd "C-x r") 'regexp-builder)

; auto-complete binding
(global-set-key [C-tab] 'ac-start)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

; speedbar
(global-set-key (kbd "M-<f2>") 'nv-speedbar-open-current-buffer-in-tree)
