;; Temp plan
;;
;;  Assuming we run our compiled program through our register machine, we would have to supply all operations used
;; by the compiled program when creating a machine that will run it.
;; But if we are going to run the compiled program similar to how we ran our interpretted programs in ch4, we would have to have
;; a list of primitive operations added to the initial environment, along with non-primitive builtins.

;; For the primitives  we need to implement, we could store them in a table like structure, and use assoc to lookup the right procedure.
;; We would add cases to the compile cond that will check if the expression is a primitive procedure (exists in our table)
;; Then we would dispatch to an appropriate code generator that is in the table element for the current primitive.

;; This is because we no longer want to lookup the primitives in the environment, we want to use our code generators.

(define (spread-operands operands)
  (if (null? operands)
    (empty-instruction-sequence)
    (let ((operand-codes (compile-operands operands)))
      (preserve-operand-codes operand-codes))))

(define (compile-operands operands)
  (if (null? (cdr operands))
    (compile (car operands) 'arg1 'next)
    (cons (compile (car operands) 'arg1 'next)
	  (spread-operands (cdr operands)))))

(define (preserve-operand-codes operand-codes)
  (if (null? (cdr operand-codes))
    (car operand-codes)
    (preserving '(arg1)
		(car operand-codes)
		(preserve-operand-codes (cdr operand-codes)))))
