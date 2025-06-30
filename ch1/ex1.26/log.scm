(require racket/trace)

;; NOTE: I have hardcoded values for a to be 5 in anticipation of the numbers 6, and 7 to be checked below.
;; This was to match up the traced process exactly with the linear version.

(define (main)
  (time-search-for-primes 1 5))

(define (time-search-for-primes n m)
  (display "average time to find ")
  (display n)
  (display " primes greater than ")
  (display m)
  (display ": ")
  (display (search-for-primes n m))
  (newline))

; searches for n primes greater than m, store them in a list
(define (search-for-primes n m)
  (search-for-primes-iter n m (list)))

(define (search-for-primes-iter n m primes-list)
  (cond ((= n 0) primes-list)
	(else
	  (define prime (find-smallest-prime-greater-than m))
	  (search-for-primes-iter (- n 1) prime (append primes-list (list prime))))))

(define (find-smallest-prime-greater-than n)
  (define times 1)
  (if (prime? (+ n 1) times) (+ n 1)
    (find-smallest-prime-greater-than (+ n 1))))

(define (prime? n times)
  ;; (define a (random 1 n))
  (define a 5)
  (cond ((= times 0) #t)
        ((= (fast-expt-mod a n) a) (prime? n (- times 1)))
	(else #f)))

(define (fast-expt-mod a n)
  (remainder (expmod-log a n n) n))

(define (expmod-log base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	  (remainder (square (expmod-log base (/ exp 2) m)) m))
	(else 
	  (remainder (* base (expmod-log base (- exp 1) m)) m))))

(trace expmod-log)

(define (square x) (* x x))
