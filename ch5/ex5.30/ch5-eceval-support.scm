;;;;SIMULATION OF ECEVAL MACHINE OPERATIONS --
;;;;loaded by load-eceval.scm and by load-eceval-compiler.scm

;;;;FIRST A LOT FROM 4.1.2-4.1.4

(load "ch5-syntax.scm");               ;section 4.1.2 syntax procedures
(load-option 'format)

;;;SECTION 4.1.3
;;; operations used by compiled code and eceval except as noted

(define (true? x)
  (not (eq? x false)))

;;* not used by eceval itself -- used by compiled code when that
;; is run in the eceval machine
(define (false? x)
  (eq? x false))

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;;(end of compound procedures)


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
        'ERROR-EXTEND-ENVIRONMENT
        'ERROR-EXTEND-ENVIRONMENT)))
          ; (error "Too many arguments supplied" vars vals)
          ; (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        'ERROR-UNBOUND-VARIABLE-IN-LOOKUP
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (list 'PERFORM-ERROR "variable:" var "is unbound")
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (if (eq? initial-env 'ERROR-EXTEND-ENVIRONMENT)
      (error "Failed to setup initial environment"))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; TODO: wrap each primitive procedure in a lambda function, that will check for applicability, and if it fails we will return a distiguishable error code to primitive apply
;; in the machine explicit scheme evaluator.

(define (make-error message)
  (cons 'ERROR message))

(define (my-car . args)
  (cond ((not (= (length args) 1)) (make-error (format #f "car expects 1 arguments, ~A were given" (length args))))
        ((null? (car args)) (make-error 'ERROR-CAR-NULL))
        ((not (pair? (car args))) (make-error 'ERROR-CAR-NOT-PAIR))
        (else (car (car args)))))

(define (my-cdr . args)
  (cond ((not (= (length args) 1)) (make-error (format #f "cdr expects 1 arguments, ~A were given" (length args))))
        ((null? (car args)) (make-error 'ERROR-CDR-NULL))
        ((not (pair? (car args))) (make-error 'ERROR-CDR-NOT-PAIR))
        (else (cdr (car args)))))

(define (my-cons . args)
  (cond ((not (= (length args) 2)) (make-error (format #f "cons expects 2 arguments, ~A were given" (length args))))
        (else (cons (car args) (cadr args)))))

(define (my-null? . args)
  (cond ((not (= (length args) 1)) (make-error (format #f "null? expects 2 arguments, ~A were given" (length args))))
        (else (null? (car args)))))

(define (generic-arithmetic proc args proc-label)
  (let ((not-nums (filter (lambda (num) (not (number? num))) args)))
    (cond ((not (null? not-nums)) (make-error (format #f "The object ~A, passed as an argument to ~A, is not the correct type" (car not-nums) proc-label)))
          (else (apply proc args)))))

(define (my-add . args)
  (if (null? args)
    (+)
    (generic-arithmetic + args "integer-add")))

(define (my-sub . args)
  (if (null? args)
    (make-error (format #f "integer-negate has been called with 0 arguments, it requires at least 1 argument"))
    (generic-arithmetic - args "integer-negate")))

(define (my-multiplier . args)
  (if (null? args)
    (*)
    (generic-arithmetic * args "*")))

(define (my-divider . args)
  (cond ((null? args) (make-error (format #f "/ has been called with 0 arguments, it requires at least 1 argument")))
        ((= (length args) 1) (generic-arithmetic / args "/"))
        ((and (number? (cadr args)) (= (cadr args) 0)) (make-error "Division by zero signalled by /"))
        (else (generic-arithmetic / args "/"))))

(define (my-integer-equals? . args)
  (if (null? args)
    (=)
    (generic-arithmetic = args "integer-equals?")))

(define (my-integer-greater? . args)
  (if (null? args)
    (>)
    (generic-arithmetic > args "integer-greater?")))

(define (my-integer-less? . args)
  (if (null? args)
    (<)
    (generic-arithmetic < args "integer-less?")))

(define primitive-procedures
  (list (list 'car my-car)
        (list 'cdr my-cdr)
        (list 'cons my-cons)
        (list 'list list) ;; doesn't need a wrapper, no arguments are required
        (list 'null? my-null?)
	;;above from book -- here are some more
	(list '+ my-add)
	(list '- my-sub)
	(list '* my-multiplier)
	(list '= my-integer-equals?)
	(list '/ my-divider)
	(list '> my-integer-greater?)
	(list '< my-integer-less?)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


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

;;; Simulation of new machine operations needed by
;;;  eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

;;; From section 5.4.4 footnote
(define (get-global-environment)
  the-global-environment)
;; will do following when ready to run, not when load this file
;;(define the-global-environment (setup-environment))


;;; Simulation of new machine operations needed for compiled code
;;;  and eceval/compiler interface (not used by plain eceval machine)
;;; From section 5.5.2 footnote
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

