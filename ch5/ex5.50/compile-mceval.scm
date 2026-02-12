(load "regsim")
(load "ch5-eceval-support")
(load "ch5-compiler.scm")

(define (main port)
  (let* ((instructions (statements (compile (read port) 'val 'return)))
         (mceval (make-machine (list
                                 (list 'lookup-variable-value lookup-variable-value)
                                 (list 'define-variable! define-variable!)
                                 (list 'make-compiled-procedure make-compiled-procedure)
                                 (list 'compiled-procedure-env compiled-procedure-env)
                                 (list 'extend-environment extend-environment)
                                 (list 'list list)
                                 (list 'primitive-procedure? primitive-procedure?)
                                 (list 'compiled-procedure-entry compiled-procedure-entry)
                                 (list 'apply-primitive-procedure  apply-primitive-procedure)
                                 (list 'false? false?)
                                 (list 'cons cons)
                               )
                               instructions)))

    (display "done compiling\n")

    ; (mceval 'enable-tracing-for-all-registers)

    (set-register-contents! mceval 'env (get-global-environment))

    (start mceval)
    ))


; (define (write-sequence file sequence)
;   (display "Writing sequence to file\n")
;   (for-each (lambda (inst) (write-line inst file)) sequence))

; (define (write-sequence file sequence)
;   (display "Writing sequence to file\n")
;   (let ((str ""))
;     (for-each (lambda (label)
;                 (write-line (car label) file)
;                 (for-each (lambda (inst) (write-line inst file)) (cdr label)))
;               sequence)))


; (define (write-sequence file sequence)
;   (display "Writing sequence to file\n")
;   (define (inner seq str)
;     (if (null? seq)
;       str
;       (inner (cdr seq) (string-append str (string (car seq)) "\n"))))
;   (define (iter seq str) 
;     (if (null? seq)
;       str
;       (let ((label (car seq)))
;         (iter (cdr seq) (string-append (string (car label)) "\n" (inner (cdr label) str))))))
;   (write-string (iter sequence "") file))

(call-with-input-file "ch4-mceval.scm" main)
