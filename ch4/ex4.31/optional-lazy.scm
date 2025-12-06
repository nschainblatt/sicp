; Example procedures to evaluate in repl:

;; note that calling non-primitive-procedure with b should leave the thunk to still be evaluated in the addition expression below it.
;; (since the non-primitive-procedure doesn't ever use b in a primitive procedure).

; (define (non-primitive-procedure x) 'ok)
; (define (f a (b lazy-memo) c (d lazy-memo))
;   (non-primitive-procedure b)
;   (+ a b c d)
;   (+ a b c d))

; (f (+ 1 1) (+ 2 2) (+ 3 3) (+ 4 4))

; (define (f a b c d) (+ 1 b))


(define (main)
  (install-eval-package)
  (driver-loop))

(define apply-in-underlying-scheme apply)

(define (println x)
  (newline)
  (display x))

(define (print label x)
  (newline)
  (display label)
  (display x))

(define the-empty-table '(head))

(define (make-table)
  (let ((table the-empty-table))

    (define (scan type sub-table)
      (cond ((null? sub-table) #f)
	    ((eq? type (caar sub-table)) (car sub-table))
	    (else (scan type (cdr sub-table)))))

    (define (lookup type proc)
      (let ((type-result (scan type (cdr table))))
	(if type-result
	  (let ((proc-result (scan proc (cdr type-result))))
	    (if proc-result
	      (cdr proc-result)
	      #f))
	  #f)))

    (define (insert type proc operation)
      (let ((type-result (scan type (cdr table))))
	(if type-result
	  (let ((proc-result (scan proc (cdr type-result))))
	    (if proc-result
	      (set-cdr! proc-result operation)
	      (set-cdr! type-result (cons (cons proc operation) (cdr type-result)))
	      ))
	  (set-cdr! table (cons (list type (cons proc operation)) (cdr table))))))

    (define (print)
      (println table))

    (define (dispatch message)
      (cond ((eq? message 'lookup) lookup)
	    ((eq? message 'insert) insert)
	    ((eq? message 'print)  print)
	    (else (error "unknown message --make-table"))))
    dispatch))

(define eval-procedure-table (make-table))

(define (get type proc)
  ((eval-procedure-table 'lookup) type proc))

(define (put type proc operation)
  ((eval-procedure-table 'insert) type proc operation))

(define (print-table)
  ((eval-procedure-table 'print)))

(define (eval exp env)
  (print "EXP: " exp)
  (let* ((type (type-tag exp))
	 (eval-handler (get 'eval type)))
    (if eval-handler
      (eval-handler exp env)
      ((get 'eval 'call) exp env))))

(define (type-tag exp)
  (cond ((pair? exp) (car exp))
	((boolean? exp) 'boolean)
	((number? exp) 'number)
	((string? exp) 'string)
	((symbol? exp) 'symbol)
	((null? exp) (error "invalid syntax null --type-tag"))
	(else (error "unknown expression type --type-tag"))))

(define contents cdr)

(define false #f)
(define true #t)
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Optional lazy evaluation with procedures:
;; 1. No changes were necessary to the syntax procedures, defining a procedure with the new syntax works fine with existing syntax
;;    procedures:
;       (define (f a (b lazy-memo) c (d lazy-memo))
;         (+ a b c d)
;         (+ a b c d))
;;
;; 2. Changes were made to the evaluation of applications, aka calling procedures.
;;    - We evaluate the operator like normal, in order to get the procedure tied to the symbol
;;    - Then we check if the procedure is primitive, if so we go through all arguments and force all of them
;;        - If any arguments are thunks, they are forced, otherwise the expression is returned.
;;    - For other procedures, we grab the parameters as well as the arguments to be applied.
;;      - They are passed to a procedure to build the thunks for the arguments whose parameters were marked as lazy, with optional memo
;;    - With the processed parameters and arguments, we create a new procedure to leave the original unmodified, and apply the
;;      the arguments to it.
;;    - apply applies the procedure like normal, thunks are not evaluated until a primitive procedure is encountered.

(define (install-eval-package)
  (define (identity exp env) exp)
  (put 'eval 'boolean identity)
  (put 'eval 'number identity)
  (put 'eval 'string identity)
  (put 'eval 'symbol lookup-variable-value)
  (put 'eval 'quote text-of-quotation)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))

  (put 'eval 'call (lambda (exp env)
		     (let ((procedure (eval (operator exp) env)))
		       (if (primitive-procedure? procedure)
			 (apply procedure (list-of-arg-values (operands exp) env))
			 (let* ((parameters (procedure-parameters procedure))
				(arguments (operands exp))
				(processed-params-args (list-of-arg-values-and-delayed-args parameters arguments env))
				(processed-params (car processed-params-args))
				(processed-args (cdr processed-params-args))
				(new-procedure (make-procedure processed-params (procedure-body procedure) (procedure-environment procedure))))
			   (apply new-procedure processed-args))))))

  (put 'eval 'let (lambda (exp env) (eval (let->combination exp) env))))

(define (list-of-arg-values-and-delayed-args parameters arguments env)
  (define (iter params args new-params new-args)
    (if (null? params)
      (cons new-params new-args)
      (let ((param (car params))
	    (arg (car args)))
	(cond ((lazy-memo? param)
	       (iter (cdr params) (cdr args) (append new-params (list (lazy-param param))) (append new-args (list (make-memo-thunk arg env)))))
	      ((lazy? param)
	       (iter (cdr params) (cdr args) (append new-params (list (lazy-param param))) (append new-args (list (make-thunk arg env)))))
	      (else (iter (cdr params) (cdr args) (append new-params (list param)) (append new-args (list (eval arg env)))))))))
  (if (= (length parameters) (length arguments))
    (iter parameters arguments '() '())
    (if (< (length parameters) (length arguments))
      (error "Too many arguments supplied" parameters arguments)
      (error "Too few arguments supplied" parameters arguments))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps)
			env)
	  (list-of-arg-values (rest-operands exps)
			      env))))

(define (actual-value exp env)
  (let ((val (eval exp env)))
    (print "val: " val)
    (force-it val env)))

(define (force-it val env)
    (cond ((memo-thunk? val)
	   (let ((inner-value (eval (thunk-arg val) env)))
	     (set-car! val 'evaluated-thunk)
	     (set-car! (cdr val) inner-value)
	     (print "inner-value: " inner-value)
	     inner-value))
	  ((thunk? val) (eval (thunk-arg val) env))
	  ((evaluated-thunk? val) (thunk-arg val))
	  (else val)))

(define (lazy-memo? exp)
  (and (pair? exp) (eq? (cadr exp) 'lazy-memo)))
(define (make-memo-thunk arg env)
  (list 'memo-thunk arg)) ;;env)) ;; TODO: may need this when forcing thunks to gurantee values will be computed with right env.
(define (memo-thunk? exp)
  (tagged-list? exp 'memo-thunk))

(define (lazy-param lazy)
  (car lazy))
(define thunk-arg cadr)

(define (lazy? exp)
  (and (pair? exp) (eq? (cadr exp) 'lazy)))
(define (make-thunk arg env)
  (list 'thunk arg)) ;;env)) ;; TODO:
(define (thunk? exp)
  (tagged-list? exp 'thunk))

(define (evaluated-thunk? exp)
  (tagged-list? exp 'evaluated-thunk))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (let* ((procedure-parts (scan-out-defines (procedure-body procedure)))
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

(define unassigned '*unassigned*)

;; body is a list of expressions
;; create a inner procedure to explore all expressions
;; it will produce three separate lists
;; the first list will be the assignments for the let, whose values will be '*unassigned
;; the second will be a list of set!s for these assignments
;; the third will be the regular expressions for the let body
;;
;; after creating these lists, append the third onto the second to ensure the set!s appear first
;; finally, return the let expression.
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
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else
	  (eval (first-exp exps) env)
	  (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp env) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (make-assignment var value)
  (list 'set! var value))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
		    ;; Note: when evaluating a procedure definition, the env is saved in the value. So if you print
		    ;; you will encounter a infinite cycle in the console.
		    (eval (definition-value exp) env)
		    env)
  'ok)

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

;;    (define (f a (b lazy) c (d lazy-memo)) . . .)

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


;; Let syntax
;; (let ((x 1) . . . (⟨varn⟩ ⟨expn⟩))
;;   ⟨body⟩)
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

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

;; TODO: maybe put a force-it in here if the value at a variable is a thunk?
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))
  (let ((value (env-loop env)))
    (if (eq? value unassigned)
      (error "Unassigned variable")
      value)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars)) (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
	  (extend-environment (primitive-procedure-names)
			      (primitive-procedure-objects)
			      the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list 'display display)
	(list 'newline newline)
	(list 'println println)
	(list 'print print)
	(list 'map map)
	(list '* *)
	(list '- -)
	(list '+ +)
	(list '= =)
	(list '> >)
	(list '< <)
	(list 'unassigned unassigned)
	;; ⟨more primitives⟩
	))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
		   (procedure-parameters object)
		   (procedure-body object)
		   '<procedure-env>))
    (display object)))

(define the-global-environment (setup-environment))
(main)
