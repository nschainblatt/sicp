;; It is impossible to create a procedure 'halts?' that detects if 'any' given procedure halts or not. However, it is possible to create this procedure for 'some' procedures,
;; just not 'all'.
;;
;; The 'try' procedure given in this problem is an example of a procedure that violates 'halts?'.
;;
;; Proof:
;; Assume that (try try) will halt, so 'halts?' evaluates to true. The consequent is an infinite loop, where 'halts?' indicated that evalutating this procedure would not
;; result in an infinite loop. This contradiction violates the expected behavior of 'halts?'.
;;
;; Assume that (try try) will not halt, 'halts?' would evaluate to false, resulting in 'halted, another contradiction.

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p) (run-forever) 'halted))
