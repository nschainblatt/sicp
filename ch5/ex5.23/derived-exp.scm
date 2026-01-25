;; Exercise 5.23: Extend the evaluator to handle derived ex-
;; pressions such as cond, let, and so on (Section 4.1.2). You
;; may “cheat” and assume that the syntax transformers such
;; as cond->if are available as machine operations.28
;;
;; Steps
;; 1. Update eval-dispatch label to have tests for cond and let expressions.
;; 2. Create labels for cond and let  derived expression evaluation named:
;;      - ev-cond
;;      - ev-let
;; 3. In these evaluation labels, they will require the derived transformation primitives:
;;      - cond->if
;;      - let->combination
;; 4. After performing the transformation, we would have to evaluate the derived expression. Note that we just have to place the derived expression in the exp register, then goto eval-dispatch.
;;      - Evaluating derived if from cond will use: ev-if
;;      - Evaluating derived combination from let will use: ev-application

;; ---

;; COND

eval-dispatch
  (test (op cond?) (reg exp))
  (branch (label ev-cond))

;; We don't need to save anything since we aren't
;; evaluating anything here, we are just transforming the cond
;; into if expressions, and then we goto evaluate the if expressions.
;; Evaluating the if expressions is where the saving of registers are used
;; as they are overwritten
;; We don't need to save exp because we don't require it later.
ev-cond
  (assign exp (op cond->if) exp)
  (goto (label eval-dispatch)) ;; Will go evaluate the nested if expressions.

;; ---

;; LET

eval-dispatch
  (test (op let?) (reg exp))
  (branch (label ev-let))

ev-let
  (assign exp (op let->combination) exp)
  (goto (label eval-dispatch)) ;; Will go evaluate the application.

;; ---

;; As you can see, derived expressions (assuming the operations are primitives provided by the machien)
;; are very simple to implement. The bulk of the work is done by these primitives and the expression evaluators
;; of the types we are transforming to.
