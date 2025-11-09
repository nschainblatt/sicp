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

(define (square x)
  (* x x))

(define (sum-of-two-squares)

  (define (square-sum-weight pair)
    (+ (square (car pair)) (square (cadr pair))))

  ;; Pairs with equal sums of squares will be next to each other because of the weighted ordering.
  (define squared-sum-pairs
    (weighted-pairs integers integers square-sum-weight))

  (define (sum-of-two-squares-filter pairs)
    (if (null? pairs)
      '()
      (let ((first (stream-car pairs))
	    (second (stream-car (stream-cdr pairs)))
	    (third (stream-car (stream-cdr (stream-cdr pairs)))))

	(if (= (square-sum-weight first) (square-sum-weight second) (square-sum-weight third))
	  (cons-stream (list (square-sum-weight first) first second third) (sum-of-two-squares-filter (stream-cdr (stream-cdr (stream-cdr pairs)))))
	  ;; Otherwise go to the next new three pairs.
	  (sum-of-two-squares-filter (stream-cdr pairs))))))

  (sum-of-two-squares-filter squared-sum-pairs))

(print-n-stream (sum-of-two-squares) 10)

;; The first 10 list of pairs of integers whose squared sum are equal.
;;
;; ((1 18) (6 17) (10 15)) ((7 26) (10 25) (14 23)) ((5 30) (14 27) (21 22)) ((1 32) (8 31) (20 25)) ((1 38) (17 34) (22 31)) ((1 43) (13 41)
;;  (25 35)) ((5 45) (23 39) (31 33)) ((1 47) (19 43) (23 41)) ((14 47) (17 46) (31 38)) ((5 50) (26 43) (34 37))
