;; The golden ratio is a fixed point of the transformation:
;; x |-> 1 + 1/x
;; 
;; This can be proven by starting with a guess of 1.
;; 1 + 1/1 = 2
;; 1 + 1/2 = 1.5
;; 1 + 1/1.5 = 1.667
;; 1 + 1/1.667 = 1.6... and so on, continually getting closer to the golden ration.
;;
;; x |-> 1 + 1/x can be rewritten as a second order polynomial: x^2 - x - 1 = 0.
;; When we solve for x in this polynomial using the quadratic formula, we get:
;; (1+-sqrt(5))/2, which is what the golden ration equals.
;;
;; Use this fact to compute the golden ration by means of teh 'fixed-point' procedure:

(define (main)
  (cube-root 27))

(define (golden-ratio)
  (display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
  (newline))

(define (sqrt x)
  (fixed-point (average-damp (lambda (guess) (/ x guess))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (guess) (/ x (square guess)))) 1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (good-enough? x y)
        (< (abs (- x y)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
         (if (good-enough? guess next)
	    next
	    (try next))))
    (try first-guess))

(define (square x) (* x x))
