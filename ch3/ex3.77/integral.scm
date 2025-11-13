;; We must use delay on the call to stream-cdr to eliminate going into an infinite loop.
;; The reason the previous version that uses scale-stream doesn't have to use delay is because it's internal stream-cdr already delayed within
;; scale-stream.
;; When you call scale-stream the stream-car is scaled first, and the stream-cdr isn't completed until the next call to stream-cdr.

(define (integral delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
	the-empty-stream
	(integral (delay (stream-cdr integrand))
		  (+ (* dt (stream-car integrand))
		     initial-value)
		  dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(newline)
(display (stream-ref (solve (lambda (y) y)
			    1
			    0.001)
		     1000))
