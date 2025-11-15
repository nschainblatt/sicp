(define (install-and-or-package)
  ;; AND
  (define and-first-exp car)
  (define and-rest-exps cdr)
  (define and-empty? null?)
  (define (and-last-operand? expressions)
    (and (not (null? expressions)) (null? (cdr expressions))))

  ;; Evaluate expressions left to right, if any are false, return false, if no arguments, return true.
  ;; If all expressions evaluate to true, return the value of the last evaluated operand.
  (define (eval-and expressions env)
    (if (and-empty? expressions)
      true ;; No expressions, return true.
      (let ((result (eval (and-first-exp expressions) env)))
	;; If all expressions are true, return value of last operand expression evaluated.
	(cond ((and (true? result) (and-last-operand expressions)) result)
	      ;; If result is true, but there are more expressions, continue.
	      ((true? result) (eval-and (and-rest-exps expressions) env))
	      (else false))))) ;; Result is false.

  ;; OR
  (define or-first-exp car)
  (define or-rest-exps cdr)
  (define or-empty? null?)

  ;; Evaluate expressions left to right, if any are true, return that value.
  ;; If all expressions evaluate to false, or if there are no expressions, return false.
  (define (eval-or expressions env)
    (if (or-empty? expressions)
      false
      (let ((result (eval (or-first-exp expressions) env)))
	(if (true? result)
	  result
	  (eval-or (or-rest-exps expressions) env)))))

  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or))
