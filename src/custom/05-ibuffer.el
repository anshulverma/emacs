;;; 05-ibuffer --- Summary
;;; Commentary:

;;; Code:

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'ibuf-ext)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

(defun ibuffer-previous-line ()
  "Wrap around to last line when going to previous line from top."
  (interactive) (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))
(defun ibuffer-next-line ()
  "Wrap around to first line when going to next line from last."
  (interactive) (next-line)
  (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))
(define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)
             (ibuffer-forward-line)))
(ad-activate 'ibuffer)

(provide '05-ibuffer)
;;; 05-ibuffer.el ends here
