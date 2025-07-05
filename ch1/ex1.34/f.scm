;; What would happen if we called (f f)?
;;
;; The mit-scheme interpreter uses applicative order evalutation. This means the arguments
;; to a procudure call are evaluated before being passed in to replace the procedures formal parameters.
;;
;; However, when we pass in 'f' as the argument to procedure 'f', the argument 'f' is already an evaluated
;; expression (we are passing a procedure as the argument, thus the procedure body will be evaluated when used later),
;; so we simply have to pass it to be applied inside the 'f' procedure body.
;; 
;; But, procedure 'f' applies it's formal parameter 'g' to a constant: 2.
;; Thus when (f f) is called, (f 2) is followed, and finally the expression (2 2) is to be evaluated.
;; However, the operator (left), 2 is not a valid procedure and thus cannot be applied to it's argument 2 (right).

(define (f g)
  (g 2))

;; Steps that take place when we call (f f)
;; (f f)
;; (define (f f) --Replace the format parameters with the argument 'f' using the substitution method.
;;   (f 2))
;; (f 2)
;; (2 2) --Evaluating (f 2) returns (2 2), and when the iterpreter attempts to evaluate this expression an error is raised,
;;         as 2 is not a valid procedure to apply to it's argument.
