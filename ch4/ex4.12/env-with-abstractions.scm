(define (main)
  (let* ((base-env the-empty-environment)
	 (second-env (extend-environment (list (make-binding 'x 1) (make-binding 'y 2) (make-binding 'z 3)) base-env))
	 (third-env (extend-environment (list (make-binding 'x 1.1)) second-env))
	 (fourth-env (extend-environment (list (make-binding 'z 3.1)) third-env)))

    (println fourth-env)
    (println (lookup-variable-value 'x fourth-env)) ;; 1.1
    (println (lookup-variable-value 'y fourth-env)) ;; 2
    (println (lookup-variable-value 'z fourth-env)) ;; 3.1

    (set-variable-value! 'x 10 fourth-env)
    (set-variable-value! 'y 20 fourth-env)
    (set-variable-value! 'z 30 fourth-env)

    (newline)
    (println fourth-env)
    (println (lookup-variable-value 'x fourth-env)) ;; 10
    (println (lookup-variable-value 'y fourth-env)) ;; 20
    (println (lookup-variable-value 'z fourth-env)) ;; 30

    (define-variable! 'a 1.5 fourth-env)
    (define-variable! 'b 2.5 fourth-env)
    (define-variable! 'c 3.5 fourth-env)

    (newline)
    (println fourth-env)
    (println (lookup-variable-value 'a fourth-env)) ;; 1.5
    (println (lookup-variable-value 'b fourth-env)) ;; 2.5
    (println (lookup-variable-value 'c fourth-env)) ;; 3.5

    ))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-binding var val)
  (cons var val))
(define binding-var car)
(define binding-val cdr)
(define (set-binding-val! binding val)
  (set-cdr! binding val))

(define (frame-bindings frame)
  frame)
(define frame-first-binding car)
(define frame-rest-bindings cdr)
(define frame-bindings-empty? null?)
(define (add-binding-to-frame! binding frame)
  (let* ((bindings (frame-bindings frame))
	 (first-binding (frame-first-binding bindings))
	 (rest-bindings (frame-rest-bindings bindings)))
    (set-car! bindings binding)
    (set-cdr! bindings (cons first-binding rest-bindings))))

(define (extend-environment bindings base-env)
  (cons bindings base-env))

;; Abstractions
(define (env-loop var env when-empty handle-result)
  (if (eq? env the-empty-environment)
    (when-empty)
    (let* ((frame (first-frame env))
	   (result (scan var (frame-bindings frame))))
      (if result
	(handle-result result)
	(env-loop var (enclosing-environment env) when-empty handle-result)))))

(define (scan var bindings)
  (cond ((frame-bindings-empty? bindings) #f)
	((eq? var (binding-var (frame-first-binding bindings))) (frame-first-binding bindings))
	(else (scan var (frame-rest-bindings bindings)))))

;; Implemented abstractions in environment operations
(define (lookup-variable-value var env)
  (env-loop
    var
    env
    (lambda () (error "Unbound variable" var))
    (lambda (result) (binding-val result))))

(define (set-variable-value! var val env)
  (env-loop
    var
    env
    (lambda () (error "Unbound variable: SET!" var))
    (lambda (result) (set-binding-val! result val))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
	 (result (scan var (frame-bindings frame))))
    (if result
      (set-binding-val! result val)
      (add-binding-to-frame! (make-binding var val) frame))))

(define (println x)
  (newline)
  (display x))

(main)
