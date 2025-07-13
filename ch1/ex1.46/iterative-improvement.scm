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
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) initial-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve-guess)
  (lambda (initial-guess)
    (define (try guess)
      (let ((new-guess (improve-guess guess)))
	 ;; NOTE: sqrt and fixed-point accept 1 and 2 arguments for their good-enough? procedures respectively.
	 ;; This may cause an issue if the arguments were provided in the wrong order.
	(if (good-enough? new-guess guess)
	  new-guess
	  (try new-guess))))
    (try initial-guess)))
