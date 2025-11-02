(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define (integers-starting-from x)
  (define integers (cons-stream x (add-streams integers ones)))
  integers)

(define factorial (cons-stream 1 (mul-streams (integers-starting-from 2) factorial)))
