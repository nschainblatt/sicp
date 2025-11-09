(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	  (let ((s1car (stream-car s1))
		(s2car (stream-car s2)))
	    (if (<= (weight s1car) (weight s2car))
	      (cons-stream
		s1car
		(merge-weighted (stream-cdr s1) s2 weight))
	      (cons-stream
		s2car
		(merge-weighted s1 (stream-cdr s2) weight)))))))

(define (pair-weight pair)
  (+ (car pair) (cadr pair)))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
		  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
		  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

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

(define (!= x y)
  (not (= x y)))

(define (<= x y)
  (or (= x y) (< x y)))

(define (divisible? x y)
  (= (remainder x y) 0))

;; a.
(print-n-stream (weighted-pairs integers integers pair-weight) 10)

;; b.
(define filtered-integers (stream-filter (lambda (x) (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5)))) integers))
(print-n-stream 
  (weighted-pairs
    filtered-integers
    filtered-integers
    (lambda (pair) (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair)))))
  10)
