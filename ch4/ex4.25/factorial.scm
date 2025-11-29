;; a. With applicative order evaluation, defining factorial with unless will lead to infinite recursion.

;; b. Yes, with normal order evaluation, this version of factorial will work as expected.

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
