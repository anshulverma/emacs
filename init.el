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
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("8c8b927e36470a3bc2b0182d8d19b815f5701cc0f306a4ccdc6a9a8a62a4bd6f" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "412c25cf35856e191cc2d7394eed3d0ff0f3ee90bacd8db1da23227cdff74ca2" "5436e5df71047d1fdd1079afa8341a442b1e26dd68b35b7d3c5ef8bd222057d1" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "05411251e1232959144334e4359f8af0931c6c1a2f3a109d0d9e6753b6dfecfe" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(fci-rule-color "#383838")
 '(global-whitespace-mode nil nil nil "enable whitespace all the time")
 '(hl-sexp-background-color "#efebe9")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (f-files "~/Dropbox/org/planner/" nil "\\.org$"))
 '(package-selected-packages
   (quote
    (sr-speedbar markdown-mode zotelo zenburn-theme window-number web-mode use-package smex smartparens prodigy popwin pallet multiple-cursors magit idle-highlight-mode htmlize helm-swoop helm-projectile helm-fuzzier helm-flx helm-dash flycheck-pos-tip flycheck-cask expand-region exec-path-from-shell drag-stuff dired-details+ company-quickhelp company-math company-flx company-dict company-auctex better-defaults)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-service 25)
 '(tab-width 2)
 '(user-full-name "Anshul Verma")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(whitespace-line-column nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
