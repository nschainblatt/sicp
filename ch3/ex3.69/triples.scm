(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) (stream-car t) x)) (stream-cdr u))
      (interleave
	(stream-map (lambda (x y) (list (stream-car s) x y)) (stream-cdr t) (stream-cdr (stream-cdr u)))
	(triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
		  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

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

(define (a2+b2=c2? triple)
  (let ((a (car triple))
       (b (cadr triple))
       (c (caddr triple)))
  (= (+ (square a) (square b)) (square c))))

(define pythagorean-triples
  (stream-filter a2+b2=c2? (triples integers integers integers)))

(define (square x)
  (* x x))

(print-n-stream (triples integers integers integers) 20)
(newline)
;; NOTE: the reason these Pythagorean triples take so long to compute is because of the order the triples are inserted into the stream.
;; It takes a while to get to subsequent triples that have i and j increasing at a rate to get a Pythagorean triples.
(print-n-stream pythagorean-triples 3)
