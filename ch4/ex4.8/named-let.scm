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
	((let? exp) (eval (let->combination exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	  (error "Unknown expression type: EVAL" exp))))

;; Let syntax
;; (let ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩))
;;   ⟨body⟩)
(define (let? exp)
  (tagged-list? exp 'let))
(define (make-let assignments body)
  (cons 'let (list assignments body)))
(define (make-named-let name assignments body)
  (cons 'let (list assignments body name)))
(define (named-let? exp)
  (not (null? (cdddr exp))))
(define (let-name exp)
  (cadddr exp))
(define (let-assignments exp)
  (cadr exp))
(define (let-body exp)
  (caddr exp))
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
	;; This produces an outer-lambda that has a bound variable that is a lambda of the body of the original parameters and body.
	;; When applied, it will immediately apply the original let body stored in the inner lambda. This inner lambda is given access
	;; to itself via the outer-lambdas bound parameter. The inner lambda may recursively call itself this way without supplying a
	;; new lambda definition because it is defined in the outer-lambdas scope.
	(cons outer-lambda Lambda))
      (cons Lambda list-of-values))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (println x)
  (newline)
  (display x))

(define example-let-expression
  (make-let (list '(x 1) '(y 2)) '(+ x y)))

;; #f
(println (named-let? example-let-expression))
;; (let ((x 1) (y 2)) (+ x y))
(println example-let-expression)
;; ((lambda (x y) + x y) 1 2)
(println (let->combination example-let-expression))
(newline)

(define example-named-let-expression
  (make-named-let
    'fib-iter
    '((a 1) (b 0) (count n))
    '(if (= count 0)
       b
       (fib-iter (+ a b) a (- count 1)))))

;; #t
(println (named-let? example-named-let-expression))
(println example-named-let-expression)
(println (let->combination example-named-let-expression))
(newline)
