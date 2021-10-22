(fset 'append '(lambda (list1 list2) (if (nilp list1)
  list2
  (cons (car list1) (append (cdr list1) list2)))))

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

; FIXME: use Emacs' backquote.el instead
(defmacro \` (form)
  (if (consp form)
    (if (and (consp (car form)) (eq (car (car form)) '\,))
      (list 'cons (cdr (car form)) (list '\` (cdr form)))
      (if (and (consp (car form)) (eq (car (car form)) '\,@))
        (list 'append (cdr (car form)) (list '\` (cdr form)))
        (list 'cons (list 'quote (car form)) (list '\` (cdr form)))))
    (list 'quote form)))
