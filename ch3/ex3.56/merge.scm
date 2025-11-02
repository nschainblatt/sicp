(define (merge s1 s2)
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else (let ((s1car (car s1))
		    (s2car (car s2)))
		(cond ((< s1car s2car) (cons-stream s1car
						    (merge (stream-cdr s1) s2)))
		      ((> s1car s2car) (cons-stream s2car
						    (merge s1 (stream-cdr s2))))
		      (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (scale-streams s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams integers ones)))

(define (println x)
  (newline) (display x))

(define S (cons-stream 1 (merge (scale-streams integers 5) (merge (scale-streams integers 3) (scale-streams integers 2)))))

(println (stream-ref S 0))
(println (stream-ref S 1))
(println (stream-ref S 2))
(println (stream-ref S 3))
(println (stream-ref S 4))
(println (stream-ref S 5))
(println (stream-ref S 6))
(println (stream-ref S 7))
(println (stream-ref S 8))
(println (stream-ref S 9))
(println (stream-ref S 10))
