(progn (fset 'sqr (lambda (x) (* x x))) (fset 'sum-sqr (lambda (n) (let ((sum 0)) (while (<= 0 (setq n (- n 1))) (setq sum (+ sum (sqr n)))) sum))) (let ((t1 (float-time))) (sum-sqr 100000) (- (float-time) t1)))