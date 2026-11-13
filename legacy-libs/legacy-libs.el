;;; legacy-libs.el --- Load the libraries kept in ~/emacs-settings/lib  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.  The library files stay in ~/emacs-settings/lib/;
;; this folder only groups the load-time references.

(load-file "~/emacs-settings/lib/typescript-mode.el")
(load-file "~/emacs-settings/lib/json-reformat.el")
(load-file "~/emacs-settings/lib/git.el") (require 'git)
(load-file "~/emacs-settings/lib/git-blame.el") (require 'git-blame)
(load-file "~/emacs-settings/lib/json.el") (require 'json)
;; (load-file "~/emacs-settings/lib/csv-mode.el")
(load-file "~/emacs-settings/lib/web-mode.el")

(provide 'legacy-libs)
;;; legacy-libs.el ends here
