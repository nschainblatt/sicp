;; In general we can make iterative processes by using ordinary procedure calls last.
;; We can make the required iteration constructs by using procedures, namely make-lambda or make-named-let to make
;; these derived expressions.

;; These are examples of real iterative constructs, not the derived expressions. These are used as a mental model when
;; building the derived expressions for our language.

(define (println x)
  (newline)
  (display x))

(define (operation x)
  (println x)
  (+ x 1))

(define (reverse-operation x)
  (println x)
  (- x 1))

(define (predicate x)
  (< x 10))

(define (do-while operation predicate . initial-variables)
  (define (iter changed-variables)
    (if (predicate changed-variables)
      (iter (operation changed-variables))
      'done))
  (iter (apply operation initial-variables)))

; (do-while operation predicate 0)

(define (while operation predicate . initial-variables)
  (define (iter . changed-variables)
    (if (apply predicate changed-variables)
      (iter (apply operation changed-variables))
      'done))
  (apply iter initial-variables))

; (while operation predicate 0)

(define (until operation predicate . initial-variables)
  (define (inverted-predicate . variables)
    (not (apply predicate variables)))
  (define (iter . changed-variables)
    (if (apply inverted-predicate changed-variables)
      (iter (apply operation changed-variables))
      'done))
  (apply iter initial-variables))

; (until reverse-operation predicate 20)

(define (for initial predicate increment operation)
  (define (iter x)
    (if (predicate x)
      (begin (operation x) (iter (+ x increment)))
      'done))
  (iter initial))

; (for 0 predicate 1 operation)

;; Derived Expressions for iterative constructs

;; WHILE
(define (make-while operation predicate initial-variables)
  (cons 'while (list operation predicate initial-variables)))
(define (while-operation exp)
  (cadr exp))
(define (while-predicate exp)
  (caddr exp))
(define (while-initial-variables exp)
  (cadddr exp))

; (define (while->combination exp)
;   (let ((predicate (while-predicate exp))
; 	(operation (while-operation exp))
; 	(initial-variables (while-initial-variables exp)))
;
;     ;; error because iter isn't defined when it's used in the body
;     (define iter (make-lambda '(changed-variables)
; 			      (make-if (cons predicate '(changed-variables))
; 				       (cons iter (cons operation '(changed-variables)))
; 				       'done)))
;     (cons iter initial-variables)))


(define (do-while->combination exp)
  (let ((predicate (while-predicate exp))
	(operation (while-operation exp))
	(initial-variables (while-initial-variables exp)))

    ;; Creates and transforms a named let to repeatedly call it's body with the changed state until the predicate is false.
    (let->combination (make-named-let 'iter
				      ;; call the operation once before the predicate
				      (list (list 'changed-variables (cons operation (list initial-variables))))
				      '(make-if (cons predicate (list changed-variables))
						(cons iter (cons operation (list changed-variables)))
						'done)))))

(define (while->combination exp)
  (let ((predicate (while-predicate exp))
	(operation (while-operation exp))
	(initial-variables (while-initial-variables exp)))

    ;; Creates and transforms a named let to repeatedly call it's body with the changed state until the predicate is false.
    (let->combination (make-named-let 'iter
				      (list (list 'changed-variables initial-variables))
				      '(make-if (cons predicate (list changed-variables))
						(cons iter (cons operation (list changed-variables)))
						'done)))))

(define (main)
  (define example-operation (make-lambda '(x) '((+ x 1))))
  (println example-operation)
  (define example-predicate (make-lambda '(x) '((< x 10))))
  (println example-predicate)
  (define example-while (make-while example-operation example-predicate 0))
  (println example-while)
  (println (do-while->combination example-while)))

;; Core
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	  (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	   (procedure-body procedure)
	   (extend-environment
	     (procedure-parameters procedure)
	     arguments
	     (procedure-environment procedure))))
	(else
	  (error
	    "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
	  (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env) (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
		    (eval (definition-value exp) env)
		    env)
  'ok)

;; Representing expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define false #f)
(define true #t)

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) ; formal parameters
		 (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
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
	(make-if (cond-predicate first)
		 (sequence->exp (cond-actions first))
		 (expand-clauses rest))))))

(define (let? exp)
  (tagged-list? exp 'let))
(define (make-let assignments body)
  (cons 'let (list '() assignments body)))
(define (make-named-let name assignments body)
  (cons 'let (list name assignments body)))
(define (named-let? exp)
  (not (null? (let-name exp))))
(define (let-name exp)
  (cadr exp))
(define (let-assignments exp)
  (caddr exp))
(define (let-body exp)
  (cadddr exp))
(define (let-first-assignment assignments)
  (car assignments))
(define (let-rest-assignments assignments)
  (cdr assignments))
(define (let-variable assignment)
  (car assignment))
(define (let-value assignment)
  (cadr assignment))
(define (empty-assignments? assignments)
  (null? assignments))

(define (let->combination exp)
  (define (let-assignments->variables-values-pair assignments)
    (define (iter variables values sub-assignments)
      (if (empty-assignments? sub-assignments)
	(cons variables values)
	(let* ((assignment (let-first-assignment sub-assignments))
	       (variable (let-variable assignment))
	       (value (let-value assignment)))
	  (iter (append variables (list variable)) (append values (list value)) (let-rest-assignments sub-assignments)))))
    (iter '() '() assignments))
  (let* ((variables-values-pair (let-assignments->variables-values-pair (let-assignments exp)))
	 (variables (car variables-values-pair))
	 (list-of-values (cdr variables-values-pair))
	 (Lambda (make-lambda variables (let-body exp))))
    (if (named-let? exp)
      (let ((outer-lambda (make-lambda (list (let-name exp)) (cons Lambda list-of-values))))
	(cons outer-lambda Lambda))
      (cons Lambda list-of-values))))

(main)
