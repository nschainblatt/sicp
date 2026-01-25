;; Exercise 5.24: Implement cond as a new basic special form
;; without reducing it to if. You will have to construct a loop
;; that tests the predicates of successive cond clauses until you
;; find one that is true, and then use ev-sequence to evaluate
;; the actions of the clause.
;;
;; Steps:
;;
;; Note that cond if like if in that the first predicate in order that is true has it's consequent
;; evaluated and returned as the value for the entire cond expression. Clauses that follow this one
;; are not tried.
;;
;; So to evaluate a cond as a special form, we must do so similar to ev-if.
;; We loop through clauses until one is true. If we reach the else clause
;; we don't have a predicate to try, and we just evaluate the expression within
;; and return that value in register val.
;;
;; We also assume we have the syntax procedures available as primitives.



;; COND as special form in Scheme
(define (eval-cond exp)
  (define (eval-clauses clauses)
    (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	  (if (null? rest)
	    (eval (sequence->exp (cond-actions first)))
	    (error "ELSE clause isn't last: COND->IF"
		   clauses))
	  (let ((pval (eval (cond-predicate first))))
	    (if pval
	      (eval (sequence->exp (cond-actions first)))
	      (eval-clauses rest)))))))
  (eval-clauses (cond-clauses exp)))

(define (true) #t)
(define (false) #f)
;; t => value2
(define t (cond ((false) 'value1)
		((true) 'value2))
		(else 'value3))
;; Error
(define t (cond ((false) 'value1)
		(else 'value3)
		((true) 'value2)))
