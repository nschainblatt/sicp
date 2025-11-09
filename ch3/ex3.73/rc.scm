(define (RC R C dt)
  (lambda (i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ C)) v0 dt))))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ x 1)) integers)))

(define RC1 (RC 5 1 0.5))

(print-n-stream (RC1 integers 1) 10)
