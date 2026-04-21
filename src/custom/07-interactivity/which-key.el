;;; which-key --- Show help popups for prefix keys  -*- lexical-binding: t; -*-

;;; Commentary:

;; Modern which-key renamed a number of variables and functions:
;;   which-key-key-replacement-alist         → gone; use which-key-replacement-alist
;;   which-key-description-replacement-alist → gone; use which-key-replacement-alist
;;   which-key-declare-prefixes              → which-key-add-key-based-replacements
;;   which-key-declare-prefixes-for-mode     → which-key-add-major-mode-key-based-replacements

;;; Code:

(require 'which-key)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-replacement-alist
        '((("<\\([[:alnum:]-]+\\)>" . nil) . ("\\1" . nil))
          (("up"                    . nil) . ("↑" . nil))
          (("right"                 . nil) . ("→" . nil))
          (("down"                  . nil) . ("↓" . nil))
          (("left"                  . nil) . ("←" . nil))
          (("DEL"                   . nil) . ("⌫" . nil))
          (("deletechar"            . nil) . ("⌦" . nil))
          (("RET"                   . nil) . ("⏎" . nil))
          ((nil . "Prefix Command") . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'")   . (nil . "λ"))
          ((nil . "/body\\'")       . (nil . "|="))
          ((nil . "\\`lunaryorn-")  . (nil . ""))
          ((nil . "projectile-")    . (nil . "proj-"))
          ((nil . "helm-")          . (nil . "h-"))
          ((nil . "magit-")         . (nil . "ma-"))))

  (which-key-add-key-based-replacements
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "helm/help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "language/spelling"
    "C-c m" "major mode"
    "C-c o" "cursors"
    "C-c o i" "cursors/insert"
    "C-c p" "projects"
    "C-c p s" "projects/search"
    "C-c p x" "projects/execute"
    "C-c p 4" "projects/other-window"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames"
    "C-c x" "text")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c m" "elisp/personal"
    "C-c m e" "eval")

  (which-key-add-major-mode-key-based-replacements 'js2-mode
    "C-c m" "js/personal"
    "C-c m r" "refactor")

  (which-key-add-major-mode-key-based-replacements 'scala-mode
    "C-c m" "scala/personal"
    "C-c m b" "scala/build")

  (which-key-add-major-mode-key-based-replacements 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-add-major-mode-key-based-replacements 'rust-mode
    "C-c C-c" "rust/cargo")

  (which-key-add-major-mode-key-based-replacements 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags"))

(provide 'which-key)
;;; which-key.el ends here
