(fset 'defmacro
  '(macro lambda (name args &rest body)
      (if (and (stringp (car body)) (cdr body))
        (setq body (cdr body))
        nil)
      (if (and (consp (car body)) (eq (car (car body)) 'declare))
        (setq body (cdr body))
        nil)
      (list 'fset (list 'quote name) (list 'quote (append (list 'macro 'lambda args) body)))))

(fset 'cadr (lambda (cons-cell) (car (cdr cons-cell))))
