(define (main)
  ((repeated (smooth square) 2) 5))

(define (repeated f n)
  (lambda (x)
    (define (recur i)
      (if (= i 1)
	f
	(compose f (recur (- i 1)))))
    ((recur n) x)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx))))))

(define (average x y z) 
  (/ (+ x y z) 3))

(define (square x) (* x x))
