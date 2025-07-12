(define (main)
  (newtons-method (cubic -12 39 -28) 9)) ;; 7, closest root to guess 9 out of the 3 total roots for this cubic.

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b  x) c)))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (good-enough? x y tolerance)
      (< (abs (- x y)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
         (if (good-enough? guess next tolerance)
	    next
	    (try next))))
    (try first-guess))
