;; Input argument 'k' must be greater than or equal to 10 to get a 4 decimal point accurate approximation to to 1/Φ.

(define (main)
  (cont-fract-iter (lambda (i) 1.0)
	      (lambda (i) 1.0)
	      10))

;; Recursive process
(define (cont-fract n d k)
  (if (= k 0)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-fract n d (- k 1))))))

;; Iterative process
(define (cont-fract-iter n d k)
  (define (iter k result)
    (if (= k 0)
      (/ (n k) result)
      (iter (- k 1) (+ (d (- k 1)) (/ (n k) result)))))
  (iter k (d k)))
