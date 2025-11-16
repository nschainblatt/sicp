;; A let* expression can be rewritten as a set of nested let expressions by placing a new let expression in the body
;; of the previous whenever a new assignment is encountered. This is repeated until there are no more assignments, where the body is
;; finally placed, like so:
;;
;; (let* ((x 1) (y (+ x 2)))
;;   <body>)
;;
;;  |
;;  v
;;
;; (let ((x 1))
;;    (let ((y (+ x 2)))
;;      ...
;;         <body>))
;;
;; Steps:
;; Extract the assignments, for each assignment, make a let where the body is a recursive call to another let, or if were on the last
;; assignment, place the body. If there were no assignments to begin with, pass null for the bindings and place the body.
(define (let*->nested-lets exp)
  (define (let*-expansion assignments)
    (cond ((let*-empty-assignments? assignments) (make-let '() (let*-body exp)))
	  ((let*-last-assignment? assignments) (make-let (list (let*-first-assignment assignments)) (let*-body exp)))
	  (else (make-let (list (let*-first-assignment assignments)) (let*-expansion (let*-rest-assignments assignments))))))
  (let*-expansion (let*-assignments exp)))

(define (let*? exp)
  (tagged-list? exp 'let*))
(define (make-let* assignments body)
  (cons 'let* (cons assignments body)))
(define (let*-assignments exp)
  (cadr exp))
(define (let*-body exp)
  (cddr exp))
(define (let*-first-assignment assignments)
  (car assignments))
(define (let*-rest-assignments assignments)
  (cdr assignments))
(define (let*-variable assignment)
  (car assignment))
(define (let*-value assignment)
  (cadr assignment))
(define (let*-empty-assignments? assignments)
  (null? assignments))
(define (let*-last-assignment? assignments)
  (null? (cdr assignments)))

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
	((let*? exp) (eval (let*->nested-lets exp) env)) ;; new let* clause
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
  (cons 'let (cons assignments body)))
(define (let-assignments exp)
  (cadr exp))
(define (let-body exp)
  (cddr exp))
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
    (cons Lambda list-of-values)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (println x)
  (newline)
  (display x))

(define example-let-expression
  (make-let (list '(x 1) '(y 2)) '(+ x y)))

;; (let ((x 1) (y 2)) (+ x y))
(println example-let-expression)
;; ((lambda (x y) + x y) 1 2)
(println (let->combination example-let-expression))
(newline)


(define example-let*-expression
  (make-let* '((x 1) (y (+ x 2))) '(+ x y)))

;; (let* ((x 1) (y (+ x 2))) + x y)
(println example-let*-expression)

;; Note that the bodies of the below let and lambda appear strange because they are inproper lists, the expression in the 
;; bodies are still intact like so: (let ((y (+ x 2))) + x y)
;;
;; (let ((x 1)) let ((y (+ x 2))) + x y)
(println (let*->nested-lets example-let*-expression))
;; ((lambda (x) let ((y (+ x 2))) + x y) 1)
(println (let->combination (let*->nested-lets example-let*-expression)))

;; It is sufficient to simply add a new let* clause whose action is:
;; (eval (let*->nested-lets exp) env)
;; This is because the result of let*->nested-lets is simply a single let, whose body may contain another let.
;; When these lets are evaluated they make calls to  let->combination, when evaluated simply invoke a lambda.
;; This will produce the expected results, a derived expression is suffice.
