(define (main)
  ((repeated square 2) 5)) ;; Should be 625

(define (repeated f n)
  (lambda (x)
    (define (recur i)
      (if (= i 1)
	f
	(compose f (recur (- i 1)))))
    ((recur n) x)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
