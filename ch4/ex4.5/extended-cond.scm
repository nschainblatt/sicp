;; The new arrow syntax for cond:

;; Notes:
;; Else is treated the same, the else clause cannot be a procedure of one argument.
;; Clauses can be a mix of the new syntax and the previous.
(cond (#f (display "this won't happen"))
      ((assoc 'b '((a 1) (b 2))) => cadr)
      (else #f))

;; Update the expand-clauses procedure to look for the type of the current clause when not an else clause, and
;; determine which type of clause it has (syntax of old or new), and handle accordingly.

;; New predicate and selectors
(define (cond-arrow-syntax? clause)
  (eq? (cadr clause) '=>))
(define (cond-arrow-syntax-test clause)
  (car clause))
(define (cond-arrow-syntax-recipient clause)
  (caddr clause))

;; Updated expand-clauses to handle new syntax
(define (expand-clauses clauses)
  (if (null? clauses)
    'false ; no else clause
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (if (cond-else-clause? first)
	(if (null? rest)
	  (sequence->exp (cond-actions first))
	  (error "ELSE clause isn't last: COND->IF"
		 clauses))
	(if (cond-arrow-syntax? first) ;; New syntax
	  ;; Notice how I didn't invoke the test yet, allowing the if statement to evaluate the expression when it needs to.
	  (make-if (cond-arrow-syntax-test first)
		   ;; When the predicate is true, call the recipient with the result.
		   (lambda () ((cond-arrow-syntax-recipient first) (cond-arrow-syntax-test first)))
		   (expand-clauses rest))
	  (make-if (cond-predicate first)
		   (sequence->exp (cond-actions first))
		   (expand-clauses rest)))))))
