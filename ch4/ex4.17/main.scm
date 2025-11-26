;; 1. Draw diagrams comparing the environments when evaluating <e3> in sequential vs simultaneous definitions.
; Answer: (see png attached)
;
;; Sequential:
; (lambda ⟨vars⟩
;   (define u ⟨e1⟩)
;   (define v ⟨e2⟩)
;   ⟨e3⟩)

;;
; Simultaneous:
; (lambda ⟨vars⟩
;   (let ((u '*unassigned*)
; 	(v '*unassigned*))
;     (set! u ⟨e1⟩)
;     (set! v ⟨e2⟩)
;     ⟨e3⟩))



;; 2. Why is there an extra frame in the transformed program?
; The extra frame in the transformed/simultaneous is from evaluating the let expression (which creates and calls another lambda, creating an additional environment).



;; 3. Explain why this difference in environment structure can never make a difference in the behavior of a correct program.
; The reason this would never break a correct program is because this transformation is done behind the scenes by the interpreter, the developer would not have
; to make any changes to their program to get the expected behavior of simultaneous assignment. This works because our interpreter creates a new frame that extends
; the enclosing environment, so the users procedure body will have access to the variables bound to the enclosing environment, with no behavior change.


;; 4. Design a way to make the interpreter implement the “simultaneous” scope rule for internal definitions without constructing the
;;    extra frame.
; Solution #1: instead of creating a let for all definitions in the lambda, we can modify the lambda before evaluating it to scan the body, and place it's definitions
; in the parameter list of the original lambda, and when applied modify the argument list in the same way.

;; Previous Version
; (lambda ⟨vars⟩
;   (let ((u '*unassigned*)
; 	(v '*unassigned*))
;     (set! u ⟨e1⟩)
;     (set! v ⟨e2⟩)
;     ⟨e3⟩))

;; New Version
; (lambda ⟨vars u v⟩
;     ⟨e3⟩)

;; To accomplish this in our interpreter, we can change procedure scan-out-defines to return a list of variables values and normal expressions.
;; This can be used during the evaluation of procedures to scan out all defines, and append them to the procedures parameters and arguments before evaluating the body.
;; The body to be evaluated will be a modified version of just normal expressions, extracted as the third item from scan-out-defines.

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 ;; Extract parts of the procedure: parameters, arguments and normal expressions
	 (let* ((procedure-parts (scan-out-defines (procedure-body procedure)))
		;; Build the modified procedure parts to call, performing the modification described above.
		(parameters (if (null? procedure-parts) (procedure-parameters procedure) (append (car procedure-parts) (procedure-parameters procedure))))
		(args (if (null? procedure-parts) arguments (append (cadr procedure-parts) arguments)))
		(body (if (null? procedure-parts) (procedure-body procedure) (caddr procedure-parts))))
	   (if (null? body)
	     (error "empty procedure body --APPLY")
	     (begin
	       (eval-sequence
		 body
		 (extend-environment
		   parameters
		   args
		   (procedure-environment procedure)))))))
	(else
	  (error
	    "Unknown procedure type: APPLY" procedure))))

(define (scan-out-defines body)
  (define (iter vars values regular sub-body)
    (if (null? sub-body)
      (if (null? vars)
	'()
	(list vars values regular))
      (let ((exp (car sub-body)))
	(if (definition? exp)
	  (iter (cons (definition-variable exp) vars) (cons (definition-value exp) values) regular (cdr sub-body))
	  (iter vars values (cons exp regular) (cdr sub-body))))))
  (iter '() '() '() body))

;; Convert this back to original form since we scan in apply now.
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
