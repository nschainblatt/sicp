(define (stream-enumerate-interval low high)
  (if (> low high) '()
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-for-each-until proc s ref)
  (cond ((null? s) 'done)
	((< ref 1) 'done)
	(else (proc (stream-car s)) (stream-for-each-until proc (stream-cdr s) (- ref 1)))))

(define (println x)
  (newline) (display x))

(define (print x)
  (display x) (display " "))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

(define (negate-series s)
  (stream-map (lambda (x) (* x -1)) s))

(define (invert-unit-series s)
  (define x (cons-stream 1 (negate-series (mul-series (stream-cdr s) x))))
  x)

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "cannot divide by zero")
    (mul-series s1 (invert-unit-series s2))))

(define (stream-map-with-index proc s)
  (define (recur sub index)
    (if (null? sub)
      '()
      (cons-stream (proc index (stream-car sub)) (recur (stream-cdr sub) (+ index 1)))))
  (recur s 1))

(define (integrate-series stream-a)
  (stream-map-with-index (lambda (index value) (* (/ 1 index) value)) stream-a))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define tangent-series (div-series sine-series cosine-series))

(newline)
(stream-for-each-until print tangent-series 20)
