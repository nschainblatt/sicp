(require racket/trace)

;; The carmichael numbers below (non-prime numbers that fool the Fermat test) appear as
;; prime even though they are not.
;;
;; These numbers completely fool the algorithm, proven below as we iterate through all possible
;; values of 'a'. These numbers range from 1 to n-1.
;;
;; This proves that no amount of Fermat tests will show that these numbers are not prime.
;;
;; For example:
;; 561 fools this test because for every number beteen 1-560 inclusive,
;; that number to the power of 561 modulo 561 equals the same number.
;; Such as 35^561 % 561 == 35.

(define (main)
  (display (prime? 561))
  (newline)
  (display (prime? 1105))
  (newline)
  (display (prime? 1729))
  (newline)
  (display (prime? 2465))
  (newline)
  (display (prime? 2821))
  (newline)
  (display (prime? 6601))
  (newline))

(define (prime? n)
  (prime?-iter n (- n 1)))

(define (prime?-iter n counter)
  (cond ((= counter 0) #t)
        ((= (fast-expt-mod counter n) counter) (prime?-iter n (- counter 1)))
	(else #f)))

(define (fast-expt-mod a n)
  (remainder (expmod-log a n n) n))

(define (expmod-log base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	  (remainder (square (expmod-log base (/ exp 2) m)) m))
	(else 
	  (remainder (* base (expmod-log base (- exp 1) m)) m))))

;; (trace expmod-log)

(define (square x) (* x x))
