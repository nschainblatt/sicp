(define (main)
  (install-eval-package)
  (driver-loop))

(define apply-in-underlying-scheme apply)

(define (println x)
  (newline)
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

(define (print)
  ((eval-procedure-table 'print)))

(define (stopwatch f)
  (let ((start (get-universal-time))
	(return-value (f)))
    (println (list "STOPWATCH: " (- (get-universal-time) start)))
    return-value))

(define (eval exp env)
  (let ((proc (analyze exp))) ;; separate out the analysis time
    (with-timings (lambda () (proc env))  ;; from the execution time
		  (lambda (run-time gc-time real-time)
		    ;; run-time should be in milliseconds (current value of ticks according to docs)
		    (println "EXECUTION TIME")
		    (println run-time)
		    (println gc-time)
		    (println real-time)
		    (println "DONE")
		    ))))

;; returns the execution procedures
(define (analyze exp)
  (with-timings (lambda ()
		  (let* ((type (type-tag exp))
			 (analysis-handler (get 'analyze type)))
		    (if analysis-handler
		      (analysis-handler exp)
		      ((get 'analyze 'call) exp)))) (lambda (run-time gc-time real-time)
						      ;; run-time should be in milliseconds (current value of ticks according to docs)
						      (println "ANALYSIS TIME")
						      (println run-time)
						      (println gc-time)
						      (println real-time)
						      (println "DONE")
						      )))

(define (type-tag exp)
  (println exp)
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

(define (install-eval-package)
  (put 'analyze 'boolean analyze-self-evaluating)
  (put 'analyze 'number analyze-self-evaluating)
  (put 'analyze 'string analyze-self-evaluating)
  (put 'analyze 'symbol analyze-variable)
  (put 'analyze 'quote analyze-quote)
  (put 'analyze 'set! analyze-assignment)
  (put 'analyze 'define analyze-definition)
  (put 'analyze 'if analyze-if)
  (put 'analyze 'lambda analyze-lambda)
  (put 'analyze 'begin (lambda (exp) (analyze-sequence (cdr exp))))
  ; (put 'analyze 'cond analyze-cond)
  (put 'analyze 'call analyze-application)
  (put 'analyze 'let analyze-let)
  )

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-quote exp)
  (let ((text (text-of-quotation exp)))
    (lambda (env) text)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(valproc (analyze (assignment-value exp))))
    (lambda (env) (set-variable-value! var
				       (valproc env)
				       env))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(valproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var
				    (valproc env)
				    env))))

(define (analyze-if exp)
  (let ((predicate (analyze (if-predicate exp)))
	(consequent (analyze (if-consequent exp)))
	(alternate (analyze (if-alternative exp))))
    (lambda (env) (if (true? (predicate env))
		    (consequent env)
		    (alternate env)))))

(define (analyze-lambda exp)
  (let ((parameters (lambda-parameters exp))
	(bodyproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure parameters bodyproc env))))

;; return a single procedure, that when invoked will execute each expression procedure sequentially.
;; turn each expression into an execution procedure
;; write a procedure that when executed calls all of those execution procedures sequentially.
(define (analyze-sequence exps)
  (println "analyzing sequence")
  (define (combine-procs-sequentially first rest)
    (if (null? rest)
      first
      (lambda (env) (first env) ((combine-procs-sequentially (car rest) (cdr rest)) env))))

  (let* ((exec-procs (map analyze exps)))
    (if (null? exec-procs)
      (error "sequence cannot be null")
      (let ((sequential-procs (combine-procs-sequentially (car exec-procs) (cdr exec-procs))))
	(lambda (env) (sequential-procs env))))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
	(fproc env)
	(map (lambda (aproc) (aproc env))
	     aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment
	    (procedure-parameters proc)
	    args
	    (procedure-environment proc))))
	(else
	  (error "Unknown procedure type: EXECUTE-APPLICATION"
		 proc))))

(define (analyze-let exp)
  (let* ((combination (let->combination exp))
	 (analyzed-lambda (analyze (car combination)))
	 (analyzed-args (map analyze (cdr combination))))
    (lambda (env)
      (execute-application
	(analyzed-lambda env)
	(map (lambda (arg) (arg env)) analyzed-args)))))


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
  ; (println "EVAL-IF")
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else
	  (eval (first-exp exps) env)
	  (eval-sequence (rest-exps exps) env))))

(define (eval-definition exp env)
  ; (println "EVAL-DEFINITION")
  (define-variable! (definition-variable exp)
		    (eval (definition-value exp) env)
		    env)
  'ok)

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (make-assignment var value)
  (list 'set! var value))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
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
  ; (println "MAKE-LAMBDA")
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
(define (make-let-assignment var val)
  (list var val))
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
	(list 'map map)
	(list '* *)
	(list '/ /)
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
