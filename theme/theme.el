;;; theme.el --- Theme setup  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.  The theme files themselves stay in
;; ~/emacs-settings/themes/.

(add-to-list 'custom-theme-load-path "~/emacs-settings/themes")
(load-theme 'afternoon t)

(set-face-attribute 'default nil :height 80)

(provide 'theme)
;;; theme.el ends here
