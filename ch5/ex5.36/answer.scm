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
