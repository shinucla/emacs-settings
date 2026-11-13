;;; diff-appearance.el --- Diff-mode face settings  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.  The file is deliberately not named diff-mode.el:
;; loading such a file would trigger `with-eval-after-load' for the symbol
;; `diff-mode' before the built-in diff-mode library is loaded.

;; diff-mode: dark red background for removed lines,
;; dark green background for added lines,
;; cyan text for hunk headers, no special highlight for "changed" lines,
;; and no highlight for indicators.
(with-eval-after-load 'diff-mode
  (setq diff-refine 'font-lock)
  (set-face-attribute 'diff-removed nil :background "#5f0000" :foreground "#bbbbbb" :extend t)
  (set-face-attribute 'diff-added nil :background "#005f5f" :foreground "#eeeeee" :extend t)
  (set-face-attribute
   'diff-indicator-removed nil :foreground "#ffffff" :background 'unspecified :inherit 'diff-removed)
  (set-face-attribute
   'diff-indicator-added nil :foreground "#44ff44" :background 'unspecified :inherit 'diff-added)

  (setq diff-refine nil) ;; to disable following change highlights
  ;; (set-face-attribute 'diff-refine-added nil :background "#0000ff" :foreground "#50afaf" :inherit nil)
  ;; (set-face-attribute 'diff-refine-removed nil :background "#ff0000" :foreground "#afaf50" :inherit nil)
  )

(provide 'diff-appearance)
;;; diff-appearance.el ends here
