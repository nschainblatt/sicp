;; The reason Louis's solution is incorrect is that it will not work with infinite streams.
;; The evaluation order of the scheme interpreter evaluated the arguments before calling the procedure. So when the
;; recursive call to pairs is called, it will invoke stream-cdr on streams s and t. With an infinite stream this will create infinite
;; (until memory runs out) recursive calls accessing the stream-cdr of s and t.
;; The original solution of pairs works because it wraps the interleave of the recursive call to pairs in the cdr of the stream to
;; be lazily evaluated.

(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		t)
    (pairs (stream-cdr s) (stream-cdr t))))

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

(print-n-stream (pairs integers integers) 1)
