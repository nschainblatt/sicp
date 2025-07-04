;; Updated the procedure 'sum' to be a linear iterative process.

;; However, I have noticed that the output of the same calls to the 'simpsonts-rule'
;; procedure are slighly different.

;; The reason for this difference is likely due to the imprecision of floating point arithmetic.
;; These two different processes (recursive in the previous exercise, and iterative in this exercise) calculate
;; the result using addition in different orders. The recursive process adds backwards
;; as it pops the latest recursive call off the call stack, adding numbers from largest to smallest.
;; The recursive process keeps a running result throughout the entire process leaving no delayed addition
;; operations to happen. This process adds numbers from smallest to largest.
;; Floating point arithmetic is not associative due to rounding errors, and therefore the results are slightly different.

(require racket/trace)

(define (main)
  (display (simpsons-rule cube 0 1 100))
  (newline)
  (display (simpsons-rule cube 0 1 1000))
  (newline)
  (display (simpsons-rule cube 0 1 10000))
  (newline))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k n) (= k 0)) y)
	  ((even? k) (* y 2))
	  (else (* y 4))))
  (* (sum term 0.0 inc n) (/ h 3)))

(define (even? x) (= (remainder x 2) 0))

(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
      result
      (sum-iter (next a) (+ (term a) result))))
  (trace sum-iter)
  (sum-iter a 0))

(define (inc x) (+ x 1))

(define (cube x) (* x x x))
