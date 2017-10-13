;;; code-folding --- Summary
;;; Commentary:

;; Code folding works differently for different major modes.
;; Here I am trying to make an attempt to configure a universal
;; method of allowing code folding.
;;
;; To use, use M-ARROW_KEYS:
;; M-<left> -- hide block
;; M-<right> -- show block
;; M-<up> -- hide all
;; M-<down> -- show all

;;; Code:

;; setup `origami'
(require 'origami)
(require 'hideshow)

(global-origami-mode)

(defvar hs-based-folding-modes-list
  '(c-mode
    emacs-lisp-mode
    java-mode
    lisp-mode
    perl-mode
    sh-mode
    json-mode)
  "Modes that should use `hide-show' mode for folding.")
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'json-mode-hook       'hs-minor-mode)

(defun av/origami-close-node ()
  "Close node using `origami'."
  (origami-close-node (current-buffer) (point)))

(defun av/origami-open-node ()
  "Open node using `origami'."
  (origami-open-node (current-buffer) (point)))

(defun av/origami-close-all-nodes ()
  "Close all using `origami'."
  (origami-close-all-nodes (current-buffer)))

(defun av/origami-open-all-nodes ()
  "Open all using `origami'."
  (origami-open-all-nodes (current-buffer)))

(defun av/hs-close-node ()
  "Close node using `hs'."
  (hs-hide-block))

(defun av/hs-open-node ()
  "Open node using `hs'."
  (hs-show-block))

(defun av/hs-close-all-nodes ()
  "Close all using `hs'."
  (hs-hide-all))

(defun av/hs-open-all-nodes ()
  "Open all using `hs'."
  (hs-show-all))

(defun av/folding (arg)
  "Fold code base don ARG."
  (interactive "P")
  (let* ((function-prefix (if (-contains? hs-based-folding-modes-list major-mode) "hs" "origami"))
         (function-suffix (cond
                           ((eq arg 1) "close-all-nodes")
                           ((eq arg 2) "open-all-nodes")
                           ((eq arg 3) "close-node")
                           ((eq arg 4) "open-node")
                           (t (error (concat "Invalid ARG: " arg)))))
         (function-name (format "av/%s-%s" function-prefix function-suffix)))
    (funcall (intern function-name))))

(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (av/folding 1)))
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (av/folding 2)))
(global-set-key (kbd "M-<left>")
                (lambda () (interactive) (av/folding 3)))
(global-set-key (kbd "M-<right>")
                (lambda () (interactive) (av/folding 4)))

(global-set-key (kbd "C-c f o") 'av/folding)
(global-set-key (kbd "C-c f u") 'origami-undo)
(global-set-key (kbd "C-c f r") 'origami-redo)

(provide 'code-folding)
;;; code-folding.el ends here
