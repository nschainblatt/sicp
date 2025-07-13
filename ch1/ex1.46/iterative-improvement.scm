(define (main)
  (display (sqrt 64)) (newline)
  (display (fixed-point (average-damp (lambda (y) (/ 64 y))) 1.0)) (newline))

(define (sqrt x)
  ((iterative-improve
     (lambda (guess) (< (abs (- (square guess) x) 0.00001)))
     (average-damp (lambda (y) (/ x y))))
   1.0))

(define (square x) (* x x))

(define tolerance 0.00001)

(define (fixed-point f initial-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) initial-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve-guess)
  (lambda (initial-guess)
    (define (try guess)
      (let ((new-guess (improve-guess guess)))
	(if (good-enough? new-guess)
	  new-guess
	  (try new-guess))))
    (try initial-guess)))
