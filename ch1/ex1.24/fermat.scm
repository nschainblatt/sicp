; I would expect the time difference between testing primes of 1000 and 1000000 to be a 2x time difference:
; log(1000) = 3 and log(1000000) = 6, 6/3 = 2.
; However the actual difference is roughly 1.5, but it still only increases by a constant amount when the number to test 
; for primality is exponentially larger.

(define (main)
  (time-search-for-primes 3 1000)
  (time-search-for-primes 3 10000)
  (time-search-for-primes 3 100000)
  (time-search-for-primes 3 1000000)
  (time-search-for-primes 3 10000000)
  (time-search-for-primes 3 100000000))

(define (time-search-for-primes n m)
  (display n)
  (display " primes larger than ")
  (display m)
  (display ": ")
  (define start-time (current-inexact-monotonic-milliseconds))
  (display (search-for-primes n m))
  (display ", took: ")
  (display (- (current-inexact-monotonic-milliseconds) start-time))
  (display " milliseconds")
  (newline))

; searches for n primes greater than m, store them in a list
(define (search-for-primes n m)
  (search-for-primes-iter n m (list)))

(define (search-for-primes-iter n m primes-list)
  (define prime (find-smallest-prime-greater-than m))
  (if (= n 0)
    primes-list
    (search-for-primes-iter (- n 1) prime (append primes-list (list prime)))))

(define (find-smallest-prime-greater-than n)
  (if (prime? (+ n 1) 5) (+ n 1)
    (find-smallest-prime-greater-than (+ n 1))))

(define (prime? n times)
  (define a (random 1 n))
  (cond ((= times 0) #t)
        ((= (fast-expt-mod a n) a) (prime? n (- times 1)))
	(else #f)))

(define (fast-expt-mod a n)
  (remainder (fast-expt-mod-iter a n n 1) n))

(define (fast-expt-mod-iter a n m p)
  (cond ((= n 0) p)
        ((even? n) (fast-expt-mod-iter (remainder (square a) m) (/ n 2) m p))
	(else (fast-expt-mod-iter a (- n 1) m (remainder (* a p) m)))))

(define (square x) (* x x))

;; (fast-expt-mod-iter 100 1009 1009 1)
;; (fast-expt-mod-iter 100 1008 1009 100)
;; (fast-expt-mod-iter 928 504 1009 100)
;; (fast-expt-mod-iter 507 252 1009 100)
;; (fast-expt-mod-iter 763 126 1009 100)
;; (fast-expt-mod-iter 985 63 1009 100)
;; (fast-expt-mod-iter 985 62 1009 627)
;; (fast-expt-mod-iter 576 31 1009 627)
;; (fast-expt-mod-iter 576 30 1009 939)
;; (fast-expt-mod-iter 824 15 1009 939)
;; (fast-expt-mod-iter 824 14 1009 842)
;; (fast-expt-mod-iter 928 7 1009 842)
;; (fast-expt-mod-iter 928 6 1009 410)
;; (fast-expt-mod-iter 507 3 1009 410)
;; (fast-expt-mod-iter 507 2 1009 16)
;; (fast-expt-mod-iter 763 1 1009 16)
;; (fast-expt-mod-iter 763 0 1009 100)
;; 100
