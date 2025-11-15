;; New special forms version
(define (install-and-or-package)
  ;; AND
  (define and-first-exp car)
  (define and-rest-exps cdr)
  (define and-empty? null?)
  (define (and-last-exp? expressions)
    (and (not (null? expressions)) (null? (cdr expressions))))
  (define (make-and expressions)
    (cons 'and expressions))

  ;; Evaluate expressions left to right, if any are false, return false, if no arguments, return true.
  ;; If all expressions evaluate to true, return the value of the last evaluated expression.
  (define (eval-and expressions env)
    (if (and-empty? expressions)
      true ;; No expressions, return true.
      (let ((result (eval (and-first-exp expressions) env)))
	;; If all expressions are true, return value of last expression expression evaluated.
	(cond ((and (true? result) (and-last-exp? expressions)) result)
	      ;; If result is true, but there are more expressions, continue.
	      ((true? result) (eval-and (and-rest-exps expressions) env))
	      (else false))))) ;; Result is false.

  ;; OR
  (define or-first-exp car)
  (define or-rest-exps cdr)
  (define or-empty? null?)
  (define (make-or expressions)
    (cons 'or expressions))

  ;; Evaluate expressions left to right, if any are true, return that value.
  ;; If all expressions evaluate to false, or if there are no expressions, return false.
  (define (eval-or expressions env)
    (if (or-empty? expressions)
      false
      (let ((result (eval (or-first-exp expressions) env)))
	(if (true? result)
	  result
	  (eval-or (or-rest-exps expressions) env)))))

  (put 'make 'and make-and)
  (put 'eval 'and eval-and)
  (put 'make 'or make-or)
  (put 'eval 'or eval-or))


;; Alternative: Derived expression versions using existing special forms
(define (install-and-or-package)
  ;; AND
  (define and-first-exp car)
  (define and-rest-exps cdr)
  (define and-empty? null?)
  (define (and-last-exp? expressions)
    (and (not (null? expressions)) (null? (cdr expressions))))

  ;; AND can be represented with nested if statements.
  (define (and->if expressions)
    (if (and-empty? expressions)
      true ;; Return true if no expressions
      (let ((first (and-first-exp expressions))
	    (rest (and-rest-exps expressions)))
	(if (and-last-exp? expressions)
	  (make-if first first false) ;; Return the value of the last expression if true, otherwise false
	  (make-if first (and->if rest) false))))) ;; Return the value of the rest of the expressions or false if current is false.

  ;; Then, AND can be evaluated like regular if statements
  (define eval-and eval-if)

  ;; OR
  (define or-first-exp car)
  (define or-rest-exps cdr)
  (define or-empty? null?)

  (define (or->if expressions)
    (if (or-empty? expressions)
      false
      (let ((first (or-first-exp expressions))
	    (rest (or-rest-exps expressions)))
	(make-if first first (or->if rest)))))
  (define eval-or eval-if)

  (put 'make 'and and->if)
  (put 'eval 'and eval-and)
  (put 'make 'or or->if)
  (put 'eval 'or eval-or))
