 ;;; which-key --- Show help popups for prefix keys.

;;; Commentary:

;;; Code:

(require 'which-key)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config

  (validate-setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order
   which-key-popup-type 'side-window
   which-key-side-window-location 'right

   ;; Let's go unicode :)
   which-key-key-replacement-alist
   '(("<\\([[:alnum:]-]+\\)>" . "\\1")
     ("up"                    . "↑")
     ("right"                 . "→")
     ("down"                  . "↓")
     ("left"                  . "←")
     ("DEL"                   . "⌫")
     ("deletechar"            . "⌦")
     ("RET"                   . "⏎"))
   which-key-description-replacement-alist
   '(("Prefix Command" . "prefix")
     ;; Lambdas
     ("\\`\\?\\?\\'"   . "λ")
     ;; Prettify hydra entry points
     ("/body\\'"       . "|=")
     ;; Drop/shorten package prefixes
     ("\\`lunaryorn-"  . "")
     ("projectile-"    . "proj-")
     ("helm-"          . "h-")
     ("magit-"         . "ma-")))

  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
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

  ;; Prefixes for major modes
  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    "C-c m" "elisp/personal"
    "C-c m e" "eval")

  (which-key-declare-prefixes-for-mode 'js2-mode
    "C-c m" "js/personal"
    "C-c m r" "refactor")

  (which-key-declare-prefixes-for-mode 'scala-mode
    "C-c C-b" "ensime/build"
    "C-c C-d" "ensime/debug"
    "C-c C-r" "ensime/refactor"
    "C-c C-v" "ensime/misc"
    "C-c m" "scala/personal"
    "C-c m b" "scala/build")

  (which-key-declare-prefixes-for-mode 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-declare-prefixes-for-mode 'rust-mode
    "C-c C-c" "rust/cargo")

  (which-key-declare-prefixes-for-mode 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags")

  :diminish which-key-mode)

(provide 'which-key)
;;; which-key.el ends here
