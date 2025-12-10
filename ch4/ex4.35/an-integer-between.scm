;; Inclusive bounds
(define (an-integer-between a b)
  (require (>= b a)) ;; Base case, once broken will go to the last branch.
  (amb a (an-integer-between (+ a 1) b))) ;; Returns a or a element from the next range.

(define (>= x y)
  (or (> x y) (= x y)))
