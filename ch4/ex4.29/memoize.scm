;; a.
;; Memoization is useful when the same delayed object is accessed more than once in a program.
;; An good example is arguments. With normal order evaluation, evaluating the arguments to be applied to a non-primitive procedure
;; are delayed, wrapped in a thunk.
;; If those arguments are used in more than one place in the procedure body (being used as arguments to other non-primitive procedures)
;; then those thunks will be passed, which are all references to the same object.
;; Without memoization, all of those usages would have to compute the expression separately, if memoization was implemented, the
;; first usage to force would update the thunk to be an evaluated thunk, replacing the expression with the forced value.
;; All other usages can use this value now instead of re-evaluating the same expression.

;; Here is an example:

(define (square x)
  (* x x))

(define (example x)
  (+ x x x))

(example (square 4))

;; In this example, a non-primitive expression is passed as an argument to the example procedure, and is delayed due to normal
;; order evaluation.
;; We perform addition on x inside the example procedure. Since addition is a primitive procedure the argument x is forced
;; to evaluate. However, without memoization we would have to compute it's value 3 times within the addition procedure.
;; With evaluation, the first x in the addition would evaluate and the rest of the x's would share it's result, saving
;; two computations. With more complex and larger programs, the difference can be drastic.

;; b.
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define (square x) (* x x))

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100 ;; for both memoized and non-memoized evaluators.
;;; L-Eval input:
count
;;; L-Eval value:
2 ;; for non-memoized (because we evaluated x in square twice, which was (id 10))
1 ;; for memoized (because we only had to evaluate (id 10) once)
