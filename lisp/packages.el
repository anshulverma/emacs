; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; list of packages I need
(setq package-list '(coffee-mode
                     markdown-mode markdown-toc
                     paredit paredit-everywhere
                     flx-ido
                     ido-vertical-mode
                     smex))

; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
