(define (stream-2 x y)
  (cons x (delay-2 y)))

(define (delay-2 expression)
  (lambda () expression))

(define (stream-car-2 s)
  (car s))

(define (stream-cdr-2 s)
  (force-2 (cdr s)))

(define (force-2 promise)
  (promise))

(define (stream-map-2 proc s)
  (if (null? s)
    '()
    (begin
      (stream-2 (proc (stream-car-2 s)) (stream-map-2 proc (stream-cdr-2 s))))))

(define (stream-for-each-2 proc s)
  (if (null? s)
    'done
    (begin
      (proc (stream-car-2 s))
      (stream-for-each-2 proc (stream-cdr-2 s)))))

(define (stream-filter-2 predicate s)
  (cond ((null? s) '())
	((predicate (stream-car-2 s)) (stream-2 (stream-car-2 s) (stream-filter-2 predicate (stream-cdr-2 s))))
	(else (stream-filter-2 predicate (stream-cdr-2 s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    '()
    (stream-2 low (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref-2 s index)
  (if (= index 0)
    (stream-car-2 s)
    (stream-ref-2 (stream-cdr-2 s) (- index 1))))

(define (display-stream s)
  (stream-for-each-2 println s))

(define (println x)
  (display x) (newline))

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map-2 accum
		(stream-enumerate-interval 1 20)))
(define y (stream-filter-2 even? seq))
(define z
  (stream-filter-2 (lambda (x) (= (remainder x 5) 0))
		   seq))
(stream-ref-2 y 7)
(display-stream z)
