;;; git-graph.el --- Display a git branch graph  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.

(defun git-graph ()
  "Display git branch graph in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*git-graph*")))
    (display-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string
                 "git log --graph --oneline --decorate --all -n 100")))
      (read-only-mode 1)
      (special-mode)
      (goto-char (point-min)))))
(provide 'git-graph)

;;; git-graph.el ends here
