;; AND
(define (and? exp)
  (tagged-list? exp 'and))
(define and-operands cdr)
(define and-first-operand car)
(define and-rest-operands cdr)
(define and-empty-operands? null?)
(define (and-last-operand? operands)
  (and (not (null? operands)) (null? (cdr operands))))

;; Evaluate operands left to right, if any are false, return false, if no arguments, return true.
;; If all operands evaluate to true, return the value of the last evaluated operand.
(define (eval-and exp env)
  (eval-and-operands (and-operands exp env)))
(define (eval-and-operands operands env)
  (if (and-empty-operands? operands)
    true ;; No operands, return true.
    (let ((result (eval (and-first-operand operands) env)))
	    ;; If all operands are true, return value of last operand expression evaluated.
      (cond ((and (true? result) (and-last-operand operands)) result)
	    ;; If result is true, but there are more operands, continue.
	    ((true? result) (eval-and-operands (and-rest-operands operands) env))
	    (else false))))) ;; Result is false.



;; OR
(define (or? exp)
  (tagged-list? exp 'or))
(define or-operands cdr)
(define or-first-operand car)
(define or-rest-operands cdr)
(define or-empty-operands? null?)

;; Evaluate operands left to right, if any are true, return that value.
;; If all operands evaluate to false, or if there are no operands, return false.
(define (eval-or exp env)
  (eval-or-operands (or-operands exp) env))
(define (eval-or-operands operands env)
  (if (or-empty-operands? operands)
    false
    (let ((result (eval (or-first-operand operands) env)))
      (if (true? result)
	result
	(eval-or-operands (or-rest-operands operands) env)))))

(define true #t)
(define false #f)
