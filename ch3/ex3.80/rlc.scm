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


(define (RLC R L C dt)
  (lambda (vC0 iL0)

    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))

    (define diL (add-streams (scale-stream iL (/ (- R) L)) (scale-stream vC (/ 1 L))))
    (define dvC (scale-stream iL (/ -1 C)))

    (cons vC iL)))



(newline)
(define test-streams ((RLC 1 1 0.2 0.1) 10 0))
(print-n-stream (car test-streams) 10)
(newline)
(print-n-stream (cdr test-streams) 10)
