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


(define (ramanujan-numbers)

  (define (cube-weight pair)
    (+ (expt (car pair) 3) (expt (cadr pair) 3)))

  ;; Pairs with equal sums of cubes will be next to each other because of the weighted ordering.
  (define cubed-pairs
    (weighted-pairs integers integers cube-weight))

  (define (ramanujan-filter pairs prev-pair)
    (cond ((null? pairs) '())
	  ((= (cube-weight (car pairs)) (cube-weight prev-pair)) (cons-stream prev-pair (ramanujan-filter (stream-cdr pairs) (car pairs))))
	  ;; We must increment the prev-pair because if there wasn't one next to it that had equal weight, the weight will be greater than current.
	  (else (ramanujan-filter (stream-cdr pairs) (car pairs)))))

  (stream-map cube-weight (ramanujan-filter (stream-cdr cubed-pairs) (stream-car cubed-pairs))))

(print-n-stream (ramanujan-numbers) 10)

;; The first 10 Ramanujan numbers:
;;
;; 1729 4104 13832 20683 32832 39312 40033 46683 64232 65728
