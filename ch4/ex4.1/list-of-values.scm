;; Always evaluates arguments passed to cons from left to right, regardless of the order of evaluation of the underlying Lisp.
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((left (eval (first-operand exps) env))
	  (right (list-of-values (rest-operands exps) env)))
      (cons left
	    right))))

;; Always evaluates arguments passed to cons from right to left, regardless of the order of evaluation of the underlying Lisp.
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((right (list-of-values (rest-operands exps) env))
	  (left (eval (first-operand exps) env)))
      (cons left
	    right))))
