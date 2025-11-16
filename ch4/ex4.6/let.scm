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
	((let? exp) (eval (let->combination exp) env)) ;; New let clause
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

;; This will convert a let expression into a list containing a lambda as the first element and
;; the arguments as the rest of the elements.
;;
;; This combination can be directly passed back to eval which will trigger the application clause (because it would trigger no other clauses
;; and is a pair), applying the lambda to the arguments.
;; Evaluating the operator would result in the lambda being evaluated, creating a procedure.
;; Each operand would be evaluated next.
;; Finally apply would be invoked applying the evaluated operands to the lambda procedure.
;;
;; Steps:
;; Grab the body of the let
;; Grab the variables of the let, separating them from the values in the assignments.
;; Create a lambda with the body and variables.
;; Return a list where the first element is the new lambda, and the rest of the values are the values of the assignments.
;;
;; Resulting syntax:
;;((lambda (⟨var_1⟩ ... ⟨var_n⟩)
;;   ⟨body⟩)
;; ⟨exp_1⟩
;; ...
;; ⟨exp_n⟩)
(define (let->combination exp)

  ;; Returns a pair containing a car of variables and a cdr of assignments, in same order.
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
  '(let ((x 1) (y 2))
     (+ x y)))

;; (let ((x 1) (y 2)) (+ x y))
(println example-let-expression)
;; ((lambda (x y) + x y) 1 2)
(println (let->combination example-let-expression))
