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
	(else (smallest-divisor-impl n (+ divisor 1)))))

(define (square x) (* x x))
