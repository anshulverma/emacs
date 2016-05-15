;;; av-setup.el --- Where all the magic begins

;;; Commentary:

;;; Code:

(require 'av-util)

;; load all files in custom folder while maintaining the order
(mapc 'load (f--files
             (expand-file-name "custom" av-src-dir)
             (s-matches? "^.*\.el$" it) t))

(provide 'av-setup)

;;; av-setup.el ends here
