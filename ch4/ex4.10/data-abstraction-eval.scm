;; Due to how eval and apply are written using custom predicates, selectors and evaluators, we can change the syntax of the language
;; by simply changing the definition of these procedures.
;; For example we can change how a quoted is represented like so:

;; From
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; To (change symbol name)
(define (quoted? exp) (tagged-list? exp 'quoted))
(define (text-of-quotation exp) (cadr exp))

;; Same for if constructs:

;; From
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

;; To (change symbol name, alter order or structure of inner expressions)
(define (make-if predicate consequent alternative)
  (list 'if  consequent predicate alternative))
(define (if? exp) (tagged-list? exp 'if-then-else))
(define (if-predicate exp) (caddr exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

;; This can be done for any procedure used in eval or apply. Just remember to change all parts of a construct (constructor, selectors, predicates)

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
