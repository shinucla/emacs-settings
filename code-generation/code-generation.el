;;; code-generation.el --- Small code generators  -*- lexical-binding: nil; -*-

;; Loaded from ~/.emacs.  These functions rely on dynamic binding.

(defun snake-to-camel (code)
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

(defun getset (class-name code)
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

(provide 'code-generation)
;;; code-generation.el ends here
