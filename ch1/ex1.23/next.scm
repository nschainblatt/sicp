; This modification from ex1.22 halves the number of steps
; to determine if a number is prime. However the time difference
; isn't exactly halved. For the last time-search-for-primes below,
; the time difference is roughly 1.28, not 2. This is likely because the
; procedure current-inexact-milliseconds is not precise with modern, faster hardware.

(define (main)
  (time-search-for-primes 3 1000)
  (time-search-for-primes 3 10000)
  (time-search-for-primes 3 100000)
  (time-search-for-primes 3 1000000)
  (time-search-for-primes 3 10000000)
  (time-search-for-primes 3 100000000)
  (time-search-for-primes 3 1000000000)
  (time-search-for-primes 3 10000000000)
  (time-search-for-primes 3 100000000000))

(define (time-search-for-primes n m)
  (display n)
  (display " primes larger than ")
  (display m)
  (display ": ")
  (define start-time (current-inexact-milliseconds))
  (display (search-for-primes n m))
  (display ", took: ")
  (display (- (current-inexact-milliseconds) start-time))
  (display " milliseconds")
  (newline))

; searches for n primes greater than m
(define (search-for-primes n m)
  (search-for-primes-iter n m (list)))

(define (search-for-primes-iter n m primes-list)
  (define prime (find-smallest-prime-greater-than m))
  (if (= n 0)
    primes-list
    (search-for-primes-iter (- n 1) prime (append primes-list (list prime)))))

(define (find-smallest-prime-greater-than n)
  (if (prime? (+ n 1)) (+ n 1)
    (find-smallest-prime-greater-than (+ n 1))))

(define (prime? n)
  (= (smallest-divisor-impl n 2) n))

(define (smallest-divisor-impl n divisor)
  (cond ((> (square divisor) n) n)
	((= (remainder n divisor) 0) divisor)
	(else (smallest-divisor-impl n (next divisor)))))

(define (square x) (* x x))

(define (next x)
  (if (= x 2)
    3
    (+ x 2)))
