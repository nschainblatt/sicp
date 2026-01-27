;; Exercise 5.25: Modify the evaluator so that it uses normal-
;; order evaluation, based on the lazy evaluator of Section 4.2.

;; What does normal order evaluation mean?
;;   Normal order evaluation means that the arguments to procedures are not evaluated until they are needed.
;;   In our language this means the arguments are not evaluated until they are used with a primitive procedure.

;; Solution:
;; Update procedure application to use thunks for compound procedures
;;   - We must delay argument expressions to compound procedures to allow them to be evaluated later
;;   - We will implement our thunks using the method in section 4.2.2 with memoization.
;;   - We will assume that delay and force are available as machine primitive operations.
;;   - We will assume that list-of-delayed-args, delay-it, force-it, actual-value, and others are availabe as machine primitive operations.
;; Steps:
;; 1. In compound-apply, instead of evaluating the sequence of argument expressions, we should build a delayed list is argument expressions using
;;    list-of-delayed-args, which builds a list of thunks.

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))

ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))

ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (assign proc (op actual-value) (reg exp) (reg env))

apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label handle-primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label handle-compound-apply))
  (goto (label unknown-procedure-type))

handle-primitive-apply
  (assign argl (op empty-arglist))
  (test (op no-operands?) (reg unev))
  (branch (label primitive-apply))

ev-appl-accumulate-forced-arg-loop
  (assign exp (op first-operand) (reg unev))
  (assign val (op actual-value) (reg exp) (reg env))
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (test (op no-operands?) (reg unev))
  (branch (label primitive-apply))
  (goto (label ev-appl-accumulate-forced-arg-loop))

primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

handle-compound-apply
  (assign argl (op empty-arglist))
  (test (op no-operands?) (reg unev))
  (branch (label compound-apply))

ev-appl-accumulate-delayed-arg-loop
  (assign exp (op first-operand) (reg unev))
  (assign val (op delay-it) (reg exp) (reg env))      ;; Delay the argument
  (assign argl (op adjoin-arg) (reg val) (reg argl))  ;; Add it to the delayed arg list
  (assign unev (op rest-operands) (reg unev))
  (test (op no-operands?) (reg unev))
  (branch (label compound-apply))                     ;; When done, go to apply with the delayed arg list in argl and operator in proc.
  (goto (label ev-appl-accumulate-delayed-arg-loop))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment) (reg unev)
                                      (reg argl)
                                      (reg env))
  (assign unev (op procedure-body) (reg proc))
  (save continue)                                      ;; We must save continue here as ev-sequence expects it to be on the stack, and we removed our
                                                       ;; previous continue save that was at the beginning of the evaluation.
  (goto (label ev-sequence))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))

ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

ev-if
  (save exp) ; save expression for later
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch)) ; evaluate the predicate

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))

ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))

ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev) ; save variable for later
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch)) ; evaluate the assignment value

ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev) ; save variable for later
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch)) ; evaluate the definition value

ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
  (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
