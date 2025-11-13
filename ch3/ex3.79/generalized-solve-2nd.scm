(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
	(add-streams (scale-stream integrand dt) int))))
  int)

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (generalized-solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(newline)
(print-n-stream (generalized-solve-2nd + 1 2 0.001) 10)
