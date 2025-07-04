;; Comparison between 'interval' and 'simpsons-rule':

;; The procedure 'simpsons-rule' provides greater accuracy for the integral
;; of cube between 0 and 1 for the same number of steps as the procedure 'integral'.
;; Run the procedure 'main' to view the difference in accuracy.

(require racket/trace)

(define (main)
  (display (integral cube 0 1 0.01))
  (newline)
  (display (simpsons-rule cube 0 1 100))
  (newline)
  (display (simpsons-rule cube 0 1 1000))
  (newline))

(define (sum-of-range a b)
  (sum identity a inc b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (even? x) (= (remainder x 2) 0))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))


;; Definite integral of a function f between the limits a and b
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))


;; Simpsons rule
(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k n) (= k 0)) y)
	  ((even? k) (* y 2))
	  (else (* y 4))))
  (* (sum term 0.0 inc n) (/ h 3)))

;; (trace sum)

