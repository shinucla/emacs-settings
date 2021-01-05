;;(add-to-list 'load-path "~/.emacs.d")
;;
;; use "TERM=xterm-256color emacs -nw" in terminal to launch emacs
;; load .emacs without restart emacs:
;;     open .emacs in a buffer
;;     M-x eval-buffer
;;

;; Melpa installation:
;; https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
;;
;;
;; M-x package-refresh-contents
;; M-x package-install RET js2-mode RET
;; M-x package-install RET rjsx-mode RET
;;
;;

(package-initialize)

(load-file "~/emacs-settings/lib/typescript-mode.el")
(load-file "~/emacs-settings/lib/json-reformat.el")
(load-file "~/emacs-settings/lib/git.el") (require 'git)
(load-file "~/emacs-settings/lib/json.el") (require 'json)
(load-file "~/emacs-settings/lib/csv-mode.el")
(load-file "~/emacs-settings/lib/web-mode.el")

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(add-hook 'web-mode-hook
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2)
	    (setq web-mode-css-indent-offset 2)
	    (setq web-mode-code-indent-offset 2)
	    (setq web-mode-indent-style 2)
	    (setq web-mode-enable-current-element-highlight t)
	    ))

(add-to-list 'custom-theme-load-path "~/emacs-settings/themes") 
(load-theme 'afternoon t)
;(load-theme 'subatomic256 t)

; (menu-bar-mode 0)
; (tool-bar-mode -1)
(auto-fill-mode -1)

(set-face-attribute 'default nil :height 80)
(setq-default indent-tabs-mode t)
(setq text-mode-hook '(lambda () (auto-fill-mode -1)))
(setq typescript-indent-level 2)
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

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key "\C-\\" 'compile-using-pipe)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
