;; I've extraced my modifications to the eceval-compiler here for easy review.
;; Run the load-eceval-compiler.scm to try it out.

;; Predicate used in eval-dispatch
(define (compile-and-run? exp)
  (tagged-list? exp 'compile-and-run))

;; Retrieve the expression to compile, it's quoted so we must gather
;; the text contents of the quotation otherwise we would compile the expression
;; as a quotation instead of it's actual type.
(define (compile-and-run-expression exp)
  (text-of-quotation (cadr exp)))

;; Compiles the scheme expression into machine language.
;; Assemble the machine language to build it into our register machine,
;; making records for the labels within.
;; Returns the instructions for the assign in the eceval to assign to register val.
;; Our compile label will then goto the contents of the val register.
(define (compile-and-run exp)
  (let ((expression (compile-and-run-expression exp)))
    (assemble (statements
                (compile expression 'val 'return))
              eceval)))

;; New operation added to the eceval machine.
(list 'compile-and-run? compile-and-run?)

;; New test to the eval-dispatch label
(test (op compile-and-run?) (reg exp))
(branch (label compile))

compile
  (assign val (op compile-and-run) (reg exp))
  (goto (reg val))
