;;; modes.el --- File modes, indentation and mode hooks  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
                                        ;(setq web-mode-indent-style 2)
            (setq web-mode-enable-current-element-highlight t)
            (setq indent-tabs-mode nil)
            ))

(auto-fill-mode -1)
(setq-default indent-tabs-mode nil)

(setq text-mode-hook '(lambda () (auto-fill-mode -1)))
(setq typescript-indent-level 2)
(setq c-basic-offset 4)
(setq css-indent-offset 4)
(setq js-indent-level 4)
(setq python-indent-level 4)
(setq python-indent-offset 4)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)
                            ))
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  indent-tabs-mode nil)
                            ))
(add-hook 'js-mode-hook (lambda ()
                          (setq c-basic-offset 4
                                indent-tabs-mode nil)
                          ))
(add-hook 'python-mode-hook
          (lambda ()
            (highlight-regexp "\t" 'hi-yellow)
            (setq c-basic-offset 4)
            (setq c-basic-level 4)
            (electric-indent-mode 1)
            (setq indent-tabs-mode nil)))

(provide 'modes)
;;; modes.el ends here
