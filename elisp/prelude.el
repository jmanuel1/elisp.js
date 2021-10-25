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

(defun reverse (the-list)
  (if (null the-list)
    nil
    (append (reverse (cdr the-list)) (list (car the-list)))))

(defun > (first &rest others)
  (if (null others)
    t
    (and (not (<= first (car others))) (apply '> others))))

; TODO: work for all types of sequences, check for circular and dotted lists
(defun length (sequence)
  (if (null sequence)
    0
    (+ 1 (length (cdr sequence)))))

; use Emacs' backquote.el
(require 'backquote)

; FIXME: optional doc parameter
(defmacro define-obsolete-function-alias (obsolete-name current-name when)
  ; do nothing
  ; TODO: aliases
  nil)

(defmacro dolist (header &rest body)
  (let ((var (car header)) (the-list (car (cdr header))) (result (car (cdr (cdr header))))
    `((let ((,var nil) (the-list ,the-list))
      (while the-list
        ,@body
        (setq the-list (cdr the-list))))
      ,result))))

(fset 'push '(macro lambda (element listname) (list 'setq listname (list 'cons element listname))))

(fset 'mapcar '(lambda (function sequence) (if
  (nilp sequence) nil
  (cons (funcall function (car sequence)) (mapcar function (cdr sequence))))))
