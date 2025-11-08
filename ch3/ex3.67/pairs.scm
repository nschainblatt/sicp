(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
      (interleave
	(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
	(pairs (stream-cdr s) (stream-cdr t))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
		 (interleave s2 (stream-cdr s1)))))

(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ x 1)) integers)))

(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(print-n-stream (pairs integers integers) 100)
