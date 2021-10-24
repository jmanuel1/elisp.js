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

(defmacro defun
  (name args &rest body)
      (if (and (stringp (car body)) (cdr body))
        (setq body (cdr body))
        nil)
      (if (and (consp (car body)) (eq (car (car body)) 'declare))
        (setq body (cdr body))
        nil)
      (if (and (consp (car body)) (eq (car (car body)) 'interactive))
        (setq body (cdr body))
        nil)
      (list 'fset (list 'quote name) (append (list 'lambda args) body)))

(defun defalias (name definition)
  ; TODO: &optional doc, defalias-fset-function property
  (fset name definition))

(defmacro unless (condition &rest forms)
  (append (list 'if condition nil) forms))

(defun not (condition) (if condition nil t))
(defalias 'null (symbol-function 'not))

; use Emacs' backquote.el
(require 'backquote)
