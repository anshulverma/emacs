; list the repositories containing them
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("elpa" . "http://tromey.com/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

;; list of packages I need
(setq package-list '(coffee-mode
                     markdown-mode markdown-toc
                     paredit paredit-everywhere
                     flx-ido
                     ido-vertical-mode
                     ido-ubiquitous
                     smex
                     projectile
                     perspective
                     persp-projectile
                     auto-complete
                     magit
                     git-messenger
                     window-number
                     undo-tree
                     ace-jump-mode
                     expand-region
                     dired-details
                     bash-completion
                     sourcemap
                     fuzzy
                     powerline
                     diminish
                     sr-speedbar
                     ample-theme
                     flycheck
                     flycheck-pos-tip
                     iedit
                     exec-path-from-shell
                     rinari
                     highlight-indentation
                     robe
                     enh-ruby-mode
                     textmate
                     rvm
                     rhtml-mode
                     yaml-mode
                     smartparens
                     web-mode
                     csv-mode
                     csv-nav
                     ox-reveal
                     htmlize
                     org-plus-contrib
                     yasnippet
                     git-timemachine
                     gnuplot-mode
                     org-mime
                     log4j-mode
                     dockerfile-mode
                     multiple-cursors
                     groovy-mode
                     auto-complete-octave
                     go-mode
                     applescript-mode
                     jade-mode))

; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
