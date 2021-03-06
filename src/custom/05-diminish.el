;;; diminish --- Summary
;;; Commentary:

;; Instead of diminish, I've used dim here for the reason listed
;; here https://github.com/alezost/dim.el#why

;; Make sure Symbola font is installed http://users.teilar.gr/~g1951d/

;;; Code:

(require 'dim)
(require 'slime)

(dim-major-names
 '((emacs-lisp-mode           "𝑬𝑳")
   (emacs-lisp-byte-code-mode "𝑬𝑳-byte")
   (lisp-mode                 "𝒍𝒊𝒔𝒑")
   (calendar-mode             "📆")
   (scheme-mode               "λ")
   (clojure-mode              "λ")
   (help-mode                 "🄷")
   (view-mode                 "👀")
   (java-mode                 "☕")
   (org-mode                  "𝒐𝒓𝒈")
   (enh-ruby-mode             "🔷")
   (ruby-mode                 "♦")))
(dim-minor-names
 '((visual-line-mode                      "")
   (auto-fill-function                    "")
   (whitespace-mode                       "" av-setup)
   (company-mode                          "" av-setup)
   (yas-minor-mode                        " 🇾" av-setup)
   (helm-mode                             "" av-setup)
   (abbrev-mode                           "" av-setup)
   (flycheck-mode                         "" av-setup)
   (flyspell-mode                         "" flyspell)
   (smartparens-mode                      "" av-setup)
   (auto-revert-mode                      "" av-setup)
   (isearch-mode                          " 🔎")
   (undo-tree-mode                        "" av-setup)
   (global-undo-tree-mode                 " ⮌" av-setup)
   (slime-mode                            "" av-setup)
   (emacs-keybinding-command-tooltip-mode "" emacs-keybinding-command-tooltip-mode)
   (av/programming-mode                   " 🖥" av-setup)
   (google-this-mode                      "")
   (clj-refactor-mode                     "" av-setup)
   (cider-mode                            "" av-setup)
   (hs-minor-mode                         "" av-setup)
   (yard-mode                             "" av-setup)
   (robe-mode                             "" av-setup)
   (highlight-symbol-mode                 "" av-setup)
   (rspec-mode                            "" av-setup)
   (buffer-face-mode                      "" av-setup)
   (ruby-tools-mode                       "" av-setup)
   (ruby-refactor-mode                    "" av-setup)
   (hi-lock-mode                          "" av-setup)))

;; make projectile mode line shorter
(setq projectile-mode-line
      '(:eval (format " [%s]" (projectile-project-name))))

(provide '05-diminish)
;;; 05-diminish.el ends here
