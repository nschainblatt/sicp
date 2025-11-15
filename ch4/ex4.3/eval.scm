(define type-tag car)
(define contents cdr)

(define (eval exp env)
  (let ((expression-handler (get 'eval (type-tag exp))))
    (cond ((expression-handler (expression-handler (contents exp) env))
	   ((applicable? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
	   (else (error 'eval "unknown expression type" exp))))))
