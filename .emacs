;;(add-to-list 'load-path "~/.emacs.d")
;; use "TERM=xterm-256color emacs -nw" in terminal to launch emacs
;; load .emacs without restart emacs:
;;     open .emacs in a buffer
;;     M-x eval-buffer
(add-to-list 'custom-theme-load-path "~/emacs-settings/themes") 
(load-theme 'afternoon t)

(load-file "~/emacs-settings/lib/typescript-mode.el")
(load-file "~/emacs-settings/lib/json-reformat.el")
(setq text-mode-hook '(lambda () (auto-fill-mode -1)))

(menu-bar-mode 0)
(tool-bar-mode -1)
(auto-fill-mode -1)

(set-face-attribute 'default nil :height 80)
(setq-default indent-tabs-mode t)
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)))
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!

(global-set-key "\M-g" 'goto-line)

(defun snake-to-camel(code)
  (setq code (replace-regexp-in-string " " "" code))
  (setq code (replace-regexp-in-string "_" " " code))
  (setq code (replace-regexp-in-string "-" " " code))

  (setq result "")
  (setq tokens (split-string code))

  (while (not (null tokens))
    (setq result (concat result (upcase-initials (car tokens))))
    (setq tokens (cdr tokens))
    )
  (setq code result)
  )

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun getset(class-name code)
  (setq code (replace-regexp-in-string "@[^ ]+" "" code))
  (setq code (replace-regexp-in-string ";" " ; " code))
  (setq code (replace-regexp-in-string "=" " = " code))
  (setq code (replace-regexp-in-string ",[ ]*" "," code))
  (setq code (replace-regexp-in-string ",[ ]*" "," code))

  (let ((tokens (split-string code))
	(lookup '())
	(getters "")
	(setters "")
	(skip nil))
    (while (not (null tokens))
      (cond ((string= "private" (car tokens)) (setq tokens (cdr tokens)))
	    ((string= "protected" (car tokens)) (setq tokens (cdr tokens)))
	    ((string= "final" (car tokens)) (setq tokens (cdr tokens)))
	    ((string= "="       (car tokens)) (setq skip t) (setq tokens (cdr tokens)))
	    ((string= ";"       (car tokens)) (setq skip nil) (setq tokens (cdr tokens)))
	    (skip (setq skip (not (string= ";" (car tokens))))
		  (setq tokens (cdr tokens)))
	    ('t (setq type (car tokens))
		(setq variable (cadr tokens))
		(setq lookup (add-to-list 'lookup (cons type variable)))
		(setq tokens (cddr tokens)))))
    (setq lookup (sort lookup (lambda (a b)
				(string< (cdr a) (cdr b)))))
    (dolist (elt lookup)
      (if (string= "boolean" (car elt))
	  (setq getters (concat getters (format "\npublic %s %s() { return %s; }"
						(car elt) (cdr elt) (cdr elt))))
	(setq getters (concat getters (format "\npublic %s get%s() { return %s; }"
					      (car elt) (upcase-initials (snake-to-camel (cdr elt))) (cdr elt)))))
      (setq setters (concat setters (format "\npublic %s set%s(%s val) { %s = val; return this; }"
					    class-name (upcase-initials (snake-to-camel (cdr elt))) (car elt) (cdr elt)))))

    (insert (concat "\n" (replace-regexp-in-string "," ", " (concat getters "\n" setters))))))

(defun seek ()
  (interactive)
  (find-file "~/projects/seek-daedalus/trunk"))

(put 'erase-buffer 'disabled nil)

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


(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
(global-set-key "\C-\\" 'compile-using-pipe)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
