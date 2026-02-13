(load "regsim")
(load "ch5-eceval-support")
(load "ch5-compiler.scm")

;; This module compiles the mceval from chapter 4 by reading it's file after I wrapped the file in a begin
;; statement. Then I appended the compiled instructions of the mymap procedure below so our interpreter can
;; use map with compiled procedures. This is because the procedures defined in the evaluators module are turned
;; into compiled procedures, which do not work with Schemes builtin map procedure.
;; Then I made a register machine using these instructions.
;; It was pretty tricky getting all this setup, since many errors don't have stack traces so tracking
;; the cause of the errors revolved a lot of print statements and inspecting the compiled code.
;; I'm sure there are other things I am forgetting to put here that were very important to set all of this up.

(define mymap
  '(define (map fn seq)
     (define (iter rest result)
       (if (null? rest)
         result
         (iter (cdr rest) (append result (list (fn (car rest)))))))
     (iter seq '())))

(define (main port)
  ;; Compile map and add to instructions so we can map using our compiled procedures.
  (let* ((instructions (append (statements (compile mymap 'val 'next)) (statements (compile (read port) 'val 'return))))
         (mceval (make-machine '(val env proc argl continue)
                   (list
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

    (set-register-contents! mceval 'env (get-global-environment))

    (start mceval)))

(call-with-input-file "ch4-mceval.scm" main)
