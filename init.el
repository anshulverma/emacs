;;; init.el --- Where all the magic begins

;;; Commentary:

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (version< emacs-version "24.4")
  (warn "Upgrade emacs version to 24.4 or higher to continue."))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("412c25cf35856e191cc2d7394eed3d0ff0f3ee90bacd8db1da23227cdff74ca2" "5436e5df71047d1fdd1079afa8341a442b1e26dd68b35b7d3c5ef8bd222057d1" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "05411251e1232959144334e4359f8af0931c6c1a2f3a109d0d9e6753b6dfecfe" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (markdown-mode zotelo zenburn-theme window-number web-mode use-package smex smartparens prodigy popwin pallet multiple-cursors magit idle-highlight-mode htmlize helm-swoop helm-projectile helm-fuzzier helm-flx helm-dash flycheck-pos-tip flycheck-cask expand-region exec-path-from-shell drag-stuff dired-details+ company-quickhelp company-math company-flx company-dict company-auctex better-defaults)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-service 25)
 '(tab-width 2)
 '(user-full-name "Anshul Verma")

 ;; this will make whitespace column length same as fill-column
 '(whitespace-line-column nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
