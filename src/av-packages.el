;;; av-packages.el --- Where all the magic begins

;;; Commentary:

;;; Code:

(require 'cl)
(require 'package)

(setq package-archives
      '(("elpy"         . "http://jorgenschaefer.github.io/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa"        . "http://melpa.org/packages/")))

(setq package-user-dir (expand-file-name "elpa" av-lib-dir))

;; we need this specific version of highlight-indentation. This is a little
;; hackery to make sure we get the version from the elpy repo.
(unless (file-directory-p (expand-file-name "elpa/highlight-indentation-0.6.0" av-lib-dir))
  (let ((package-archives '(("elpy" . "http://jorgenschaefer.github.io/packages/"))))
    (package-initialize)
    (package-refresh-contents)
    (package-install 'highlight-indentation)))

(package-initialize)

(defvar av/packages
  (list 'flx
        'flx-ido
        'ido-completing-read+
        'ido-vertical-mode
        'ido-at-point

        'smex
        'yasnippet
        'flycheck 'flycheck-pos-tip
        'magit
        'exec-path-from-shell
        'zenburn-theme 'leuven-theme 'ample-theme
        'htmlize 'web-mode

        'company

        ;; latex stuff
        'company-auctex
        'auctex 'reftex

        ;; org mode additions
        'org-plus-contrib
        'org-ref
        'cm-mode
        'ox-twbs ; export org-mode docs as html compatible with twitter bootstrap
        'org-present ; ultra-minimalist presentation minor-mode for emacs org-mode
        'ob-http
        'org-bullets ; show org-mode bullets as UTF-8 characters
        'ox-ioslide
        'org-page

        'howm ; wiki like not taking tool

        'gnuplot
        'graphviz-dot-mode

        'company-flx 'company-math
        'company-dict 'company-quickhelp
        'undo-tree
        'dim
        'eimp ; image manipulation library

        'elpy
        'pydoc
        'python-mode
        'py-autopep8
        'company-jedi
        'ein ; emacs ipython notebook support
        'ob-ipython
        'pyenv-mode
        ;; 'jedi 'jedi-direx

        'helm 'helm-themes 'helm-bibtex
        'helm-swoop 'helm-flx 'helm-fuzzier
        'helm-dash

        'f         ; file functions https://github.com/rejeep/f.el
        's         ; string functions
        'dash      ; list functions
        'ht        ; hash functions
        'rainbow-mode

        'ace-jump-mode
        'ace-isearch

        'use-package

        'hydra
        'key-chord

        'git-timemachine
        'button-lock
        'elfeed
        'expand-region
        'idle-highlight-mode
        'multiple-cursors
        'popwin
        'prodigy
        'projectile
        'smartparens
        'window-number
        'better-defaults
        'info-look
        'undo-tree
        'iedit
        'slime
        'origami

        ;; clojure
        'clojure-mode
        'cider
        'clj-refactor
        'clojure-snippets

        ;; markdown
        'markdown-mode
        'markdown-toc

        'validate
        'which-key

        ;; useful additions to eshell
        'eshell-did-you-mean
        'eshell-z

        ;; google packages
        'google-maps
        'google-translate
        'google-this

        'calfw
        'calfw-org
        'calfw-cal

        'websocket

        ;; R
        'ess

        ;; docker
        'docker
        'dockerfile-mode

        'groovy-mode

        ;; REST with emacs
        'cl-lib
        'restclient
        'ob-restclient
        'company-restclient
        'know-your-http-well

        ;; big data
        'pig-mode
        'pig-snippets

        ;; dir tree view like vim
        'neotree

        ;; highlight symbol under point
        'highlight-symbol

        ;; ruby mode setup
        'yard-mode ; highlights yard document tags
        'robe ; code assistance tool
        'rvm ; emacs integration with rvm
        'enh-ruby-mode ; enhanced ruby mode
        'rspec-mode ; convenience functions for dealing with RSpec
        'ruby-tools ; collection of handy functions for ruby mode
        'ruby-additional ; ruby extensions yet to be merged into emacs
        'ruby-hash-syntax ; toggle ruby hash syntax between classic and 1.9 styles
        'ruby-refactor ; minor mode for ruby refactoring utilities

        ;; highlight uncommitted changes
        'diff-hl

        ;; attach to a debugger process (like pdb)
        'realgud)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p av/packages)
  (package-refresh-contents)
  (dolist (package av/packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

(message (format "%d packages configured" (length av/packages)))

;; Make sure some packages are byte-compiled. This should only happen the first
;; time. I committed some packages to git without .elc files, so it seems like
;; this is a good idea.
(loop for library in '("org" "org-plus-contrib")
      do
      (when (locate-library library)
        (unless (string= "elc" (file-name-extension (locate-library library)))
          (byte-recompile-directory
           (file-name-directory (locate-library library))
           0))))

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(provide 'av-packages)

;;; av-packages.el ends here
