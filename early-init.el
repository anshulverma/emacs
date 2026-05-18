;;; early-init.el --- Pre-package, pre-UI setup  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before init.el on Emacs 27+. The goal here is startup speed:
;; postpone garbage collection, skip the default package auto-init
;; (av-packages.el calls package-initialize explicitly), and disable
;; UI chrome before the frame is created so we don't paint it twice.

;;; Code:

;; Postpone GC until after startup; let av-setup.el / individual
;; modes reset to a sane value once the user is actually typing.
(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)

(defvar av--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist av--initial-file-name-handler-alist)))

;; We call (package-initialize) explicitly from av-packages.el after
;; configuring archives; skip Emacs 27+'s automatic call.
(setq package-enable-at-startup nil)

;; Async native-compilation of third-party packages (helm-flx,
;; helm-fuzzier, flx, ...) emits byte-compile warnings we can't fix
;; without editing vendored source. Keep them in the log, but don't
;; let the *Warnings* buffer pop up. Emacs 28+; harmless to set on 27.
(setq native-comp-async-report-warnings-errors 'silent)

;; Skip the site-start startup banner; save a full directory walk.
(setq site-run-file nil)

;; Kill UI chrome before the frame is drawn.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resize frames by pixel, not char — avoids a flash on themes that
;; change line-height.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
