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
  "Customizations for Emacs setup.")

(add-to-list 'load-path av-lib-dir)
(add-to-list 'load-path av-src-dir)

(require 'av-packages)
(require 'av-setup)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(package-selected-packages
   (quote
    (zotelo zenburn-theme window-number web-mode use-package smex smartparens prodigy popwin pallet multiple-cursors magit idle-highlight-mode htmlize helm-swoop helm-projectile helm-fuzzier helm-flx helm-dash flycheck-pos-tip flycheck-cask expand-region exec-path-from-shell drag-stuff dired-details+ company-quickhelp company-math company-flx company-dict company-auctex better-defaults)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-service 25)
 '(tab-width 2)
 '(user-full-name "Anshul Verma"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
