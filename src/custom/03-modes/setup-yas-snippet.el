;;; setup-yas-snippet --- Summary
;;; Commentary:
;;; Code:

;; Completing point by some yasnippet key
(require 'yasnippet)

(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-ido-expand)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/custom")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/AndreaCrotti")

(yas-reload-all)

(add-hook 'org-mode-hook 'yas-minor-mode)

(provide 'setup-yas-snippet)
;;; setup-yas-snippet.el ends here
