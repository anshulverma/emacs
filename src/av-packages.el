;;; av-packages.el --- Where all the magic begins

;;; Commentary:

;;; Code:

(require 'cl)
(require 'package)

(setq package-archives
      '(("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(setq package-user-dir (expand-file-name "elpa" av-lib-dir))

;; we need this specific version of highlight-indentation. This is a little
;; hackery to make sure we get the version from the elpy repo.
(unless (file-directory-p (expand-file-name "elpa/highlight-indentation-0.6.0" av-lib-dir))
  (let ((package-archives '(("elpy" . "http://jorgenschaefer.github.io/packages/"))))
    (package-initialize)
    (package-refresh-contents)
    (package-install 'highlight-indentation)))

(package-initialize)

(defvar starter-kit-packages
  (list 'flx-ido 'ido-ubiquitous 'ido-vertical-mode
        'smex
        'dired-details 'dired-details+
        'yasnippet
        'flycheck 'flycheck-pos-tip
        'magit
        'exec-path-from-shell
        'zenburn-theme 'leuven-theme 'ample-theme
        'htmlize 'web-mode
        'auctex 'reftex
        'org-ref 'cm-mode
        'company 'company-auctex
        'company-flx 'company-math
        'company-dict 'company-quickhelp
        'undo-tree
        'dim
        'eimp
        'elpy 'pydoc 'python-mode
        'org-plus-contrib
        'jedi 'jedi-direx
        'helm 'helm-themes 'helm-bibtex
        'helm-swoop 'helm-flx 'helm-fuzzier
        'helm-dash 'helm-projectile
        'f			 ; file functions https://github.com/rejeep/f.el
        's			 ; string functions
        'dash		 ; list functions
        'ht			 ; hash functions
        'rainbow-mode
        'ace-jump-mode 'ace-isearch
        'use-package
        'hydra 'key-chord
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
        'iedit)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p starter-kit-packages)
  (package-refresh-contents)
  (dolist (package starter-kit-packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

;; Make sure some packages are byte-compiled. This should only happen the first
;; time.  I committed some packages to git without .elc files, so it seems like
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
