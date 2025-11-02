(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams integers ones)))

;; s0, s0 + s1, s0 + s1 + s2
(define (partial-sums s)
  (define partial-sum (cons-stream (stream-car s)
				   (add-streams partial-sum (stream-cdr s))))
  partial-sum)

(define (println x)
  (display x) (newline))

(define s (partial-sums integers))

(newline)
(println (stream-ref s 0))
(println (stream-ref s 1))
(println (stream-ref s 2))
(println (stream-ref s 3))
(println (stream-ref s 4))

