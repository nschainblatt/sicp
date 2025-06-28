(define (main)
  (display (smallest-divisor 199))
  (newline)
  (display (smallest-divisor 1999))
  (newline)
  (display (smallest-divisor 19999))
  (newline))

(define (smallest-divisor n)
  (smallest-divisor-impl n 2))

(define (square x) (* x x))

(define (smallest-divisor-impl n divisor)
  (cond ((> (square divisor) n) n)
	((= (remainder n divisor) 0) divisor)
	(else (smallest-divisor-impl n (+ divisor 1)))))
