;;; editing.el --- Completion, buffer and line editing commands  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.

(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!

(global-set-key "\M-g" 'goto-line)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun programmings ()
  (interactive)
  (find-file "~/programmings")
  (set-branch-in-mode-line "programmings")
  (message "programmings"))

(defun compile-using-pipe ()
  (interactive)
  (let ((process-connection-type nil))
    (call-interactively 'compile)))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(put 'erase-buffer 'disabled nil)

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key "\C-\\" 'compile-using-pipe)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

(provide 'editing)
;;; editing.el ends here
