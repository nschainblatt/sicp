(require racket/trace)

(define (main)
  (display (factorial 0))
  (newline)
  (display (factorial 1))
  (newline)
  (display (factorial 2))
  (newline)
  (display (factorial 3))
  (newline)
  (display (factorial 4))
  (newline)
  (display (factorial 5))
  (newline)
  (display (PI 1000))
  (newline)
  (display (PI 10000))
  (newline)
  (display (PI 100000))
  (newline)
  (display (PI 1000000))
  (newline)
  (display (PI 10000000))
  (newline))

;; Create the product higher order procedure, recursively first.
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

;; Use the product procedure to define factorial.
(define (factorial n)
  (product identity 1 inc n))

;; Use the product procedure to define PI, to calculate approximations to PI.
(define (PI n)
  (define (term a)
    (cond ((even? a) (/ (+ a 2) (+ a 1)))
	  (else (/ (+ a 1) (+ a 2)))))
  (* (product-iter term 1.0 inc n) 4))

;; Create a version of the product procedure that has a iterative process.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (inc x ) (+ x 1))

(define (even? x) (= (remainder x 2) 0))
