;;; av-packages.el --- Where all the magic begins

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'package)

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")))

;; Package availability varies by archive: many useful packages
;; only exist on melpa (rolling), not melpa-stable. Giving melpa the
;; top priority means package.el will happily install anything it
;; finds, while still preferring GNU/NonGNU ELPA when they publish
;; the same package.
(setq package-archive-priorities
      '(("gnu"          . 10)
        ("nongnu"       . 8)
        ("melpa"        . 5)
        ("melpa-stable" . 1)))

(setq package-user-dir (expand-file-name "elpa" av-lib-dir))

(package-initialize)

(defvar av/packages
  (list 'flx                            ; used by helm-flx and helm-smex
        'smex                           ; ranking backend for helm-smex
        'yasnippet
        'flycheck 'flycheck-pos-tip
        'magit
        'exec-path-from-shell
        'zenburn-theme 'leuven-theme 'ample-theme
        'htmlize 'web-mode

        'company

        ;; latex stuff  (reftex and info-look are built-in)
        'company-auctex
        'auctex

        ;; org mode additions. `org' ships with Emacs, but GNU ELPA
        ;; tracks a newer release; `org-contrib' replaces the old
        ;; `org-plus-contrib' meta-package (retired in 2021).
        'org
        'org-contrib
        'org-ref
        'cm-mode
        'org-present ; ultra-minimalist presentation minor-mode for emacs org-mode
        'org-bullets ; show org-mode bullets as UTF-8 characters

        'gnuplot
        'graphviz-dot-mode

        'company-flx 'company-math
        'company-dict 'company-quickhelp
        'undo-tree
        'dim

        ;; python — built-in python.el + eglot + apheleia. Former
        ;; stack (elpy, python-mode, py-autopep8, company-jedi, ein,
        ;; pyenv-mode, jedi) removed.
        'apheleia
        'pydoc

        'helm 'helm-bibtex
        'helm-flx 'helm-fuzzier
        'helm-dash

        'f                              ; file functions
        's                              ; string functions
        'dash                           ; list functions
        'ht                             ; hash functions
        'rainbow-mode

        'use-package

        'hydra
        'key-chord

        'git-timemachine
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
        'iedit
        'slime                          ; referenced in 05-diminish.el
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

        'google-this                    ; activated in 05-editor.el

        'calfw
        'calfw-org
        'calfw-cal

        'websocket

        'ess                            ; R

        'docker
        'dockerfile-mode

        'groovy-mode

        ;; REST with emacs
        'restclient
        'ob-restclient
        'company-restclient
        'know-your-http-well

        'highlight-symbol

        ;; ruby mode setup
        'yard-mode                      ; highlights yard document tags
        'robe                           ; code assistance tool
        'rvm                            ; emacs integration with rvm
        'enh-ruby-mode
        'rspec-mode
        'ruby-tools
        'ruby-hash-syntax
        'ruby-refactor

        'diff-hl                        ; highlight uncommitted changes
        'realgud                        ; attach to a debugger like pdb

        ;; AI assistants
        'gptel                          ; Claude/OpenAI/etc chat client
        'eat)                           ; pure-elisp terminal, used by claude-code-ide
  "Libraries that should be installed by default.

Pruned 2026-04 — removed packages that were dead, unavailable, or
unused:
  - cl-lib, reftex, info-look: shipped with Emacs
  - helm-themes, howm: unavailable / broken on modern Emacs
  - ace-jump-mode, ace-isearch: unused (superseded by avy)
  - ob-http, ox-twbs, ox-ioslide, org-page, eimp, neotree,
    button-lock, google-maps, google-translate, ruby-additional,
    ob-ipython, pig-mode, pig-snippets: no references in src/custom
  - jedi-direx: already commented out.")

(unless (cl-every #'package-installed-p av/packages)
  (package-refresh-contents)
  (dolist (package av/packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package)
      ;; package-install byte-compiles the new package against whatever
      ;; is currently on load-path — which for `org' / `org-contrib'
      ;; means the *built-in* Org is still ahead, so the fresh .elc
      ;; bakes in the old `org-release' string and triggers "Org
      ;; version mismatch" warnings on every subsequent startup.
      ;; Re-byte-compile now that the ELPA copy is activated.
      (when (memq package '(org org-contrib))
        (let ((dir (file-name-directory (locate-library (symbol-name package)))))
          (byte-recompile-directory dir 0 t))))))

(message (format "%d packages configured" (length av/packages)))

;; Ensure org / org-contrib are byte-compiled at least once. The loop
;; above handles the stale-version case on first install; this handles
;; the case where the repo was ever checked in without .elc files.
(cl-loop for library in '("org" "org-contrib")
         do
         (when (locate-library library)
           (unless (string= "elc" (file-name-extension (locate-library library)))
             (byte-recompile-directory
              (file-name-directory (locate-library library))
              0))))

(provide 'av-packages)

;;; av-packages.el ends here
