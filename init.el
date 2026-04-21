;;; init.el --- Where all the magic begins  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (version< emacs-version "27.1")
  (warn "This config expects Emacs 27.1 or newer; older versions may not work."))

;; remember this directory
(defconst av-basedir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where Emacs configuration lives.")

(defvar av-src-dir (expand-file-name "src" av-basedir)
  "Customizations for Emacs setup.")

(defvar av-lib-dir (expand-file-name "lib" av-basedir)
  "Useful library code found around the web.")

(add-to-list 'load-path av-lib-dir)
(add-to-list 'load-path (expand-file-name "jmax" av-lib-dir))

(add-to-list 'load-path av-src-dir)
(require 'av-packages)
(require 'av-setup)

;; Keep Customize's auto-generated state out of this file so it stops
;; fighting hand-edits in src/custom/*.el and init.el.
(setq custom-file (expand-file-name "custom.el" av-basedir))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
