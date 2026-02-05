;; The compiler evaluates operands in right-to-left order.
;; This is because we reverse the operand-codes list in the
;; procedure 'construct-arglist' before we place them in the
;; compiled output.

;; The operand-codes are reversed so that the procedure 'code-to-get-rest-args'
;; can construct the args list using cons.

;; Remember that using cons to build up a list will leave the items in reversed order, so we
;; reverse the operand-codes list ahead of time so we can cons up each argument in constant time.

;; If we didn't reverse the operand-codes during compile time, we would be forced to use append to build up the argument list.
;; Remember that appending two lists together cdrs down the first until the last pair is reached before consing the second list to the last cdr.
;; The time complexity of append is linearly based on the size of the first list passed to append.

;; To change the order of evaluation to left-to-right, I will update 'construct-arglist' to no longer reverse the order or the operand-codes.
;; I will also update 'code-to-get-rest-args' to use append instead of cons.

;; Not only does append have a higher time complexity, it also requires an extra instruction to place the val or the evaluated operand in a list
;; to work correctly with append.

;; The original implementation only had to reverse the list once.
;; This new implementation doesn't have to reverse the list but has to append each operand onto the argument list.

;; Modifications to the compiler to get left-to-right order of evaluation:
(define (construct-arglist operand-codes)
  ;; No longer reversing the operand codes
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
       '((assign argl (const ()))))
      (let ((code-to-get-first-arg ;; Changes to first-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
               '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-first-arg
            (preserving '(env)
             code-to-get-first-arg
             (code-to-get-rest-args
              (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(val argl)
           '((assign val (op list) (reg val)) ;; We must place val in a list in order to use with append.
             (assign argl (op append) (reg argl) (reg val)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;; Example code to test with
(compile '(+ 1 2)
         'val
         'next)

;; Left-to-right order of evaluation with my changes
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (const 2))
(assign val (op list) (reg val)) ;; This is the extra instruction that is required with append.
(assign argl (op append) (reg argl) (reg val))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1

;; Right-to-left order of evaluation with the original compiler
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 2))
(assign argl (op list) (reg val))
(assign val (const 1))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1
