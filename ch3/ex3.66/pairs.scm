;; The first pair (1 1) is placed in the first section, the rest of the pairs that are placed in the second and third sections
;; alternating.

;; Whenever a new row is introduced, such as with pairs (2 2) and (3 3), the recursive call to pairs makes more interleave calls, which
;; basically divide the new row into three sections like with the first row. So with each new row, more alternating parts get pairs at 
;; each step. Since each row contains additional alternating pair placements, the progress made in the third section slows much faster
;; than the second section.
;; Because at each step you are still alternating between the second sections in all rows, and only placing pairs that may eventually lead
;; to new rows much less often.

;; Each new row gets updated half as often as it's previous row, this continues up all the way to the first row, which is updated every
;; other placement.

;; So to count pairs that precede a pair following patter (1, n) is fairly straightforward since that pair is reached must faster than
;; with a larger number in the first place in the pair (such as (100 100)).

;; The pair count that precedes pair (1, 100) is 197.

;; The pair counts that precede pairs (99, 100) and (100, 100) is insanely large.

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

(print-n-stream (pairs integers integers) 198)
