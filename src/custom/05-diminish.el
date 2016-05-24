;;; diminish --- Summary
;;; Commentary:

;; Instead of diminish, I've used dim here for the reason listed
;; here https://github.com/alezost/dim.el#why

;; Make sure Symbola font is installed http://users.teilar.gr/~g1951d/

;;; Code:

(require 'dim)

(dim-major-names
 '((emacs-lisp-mode           "𝑬𝑳")
   (emacs-lisp-byte-code-mode "𝑬𝑳-byte")
   (lisp-interaction-mode     "𝒍𝒊𝒔𝒑")
   (calendar-mode             "📆")
   (scheme-mode               "λ")
   (help-mode                 "🄷")
   (view-mode                 "👀")))
(dim-minor-names
 '((visual-line-mode      "")
   (auto-fill-function    "")
   (whitespace-mode       "" av-setup)
   (company-mode          "" av-setup)
   (yas-minor-mode        " 🇾" av-setup)
   (helm-mode             "" av-setup)
   (abbrev-mode           "" av-setup)
   (flycheck-mode         "" av-setup)
   (flyspell-mode         "" flyspell)
   (smartparens-mode      "" av-setup)
   (auto-revert-mode      "" av-setup)
   (isearch-mode          " 🔎")
   (undo-tree-mode        "" av-setup)
   (global-undo-tree-mode " ⮌" av-setup)))

(provide '05-diminish)
;;; 05-diminish.el ends here
