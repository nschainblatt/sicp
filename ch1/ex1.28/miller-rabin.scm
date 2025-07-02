(require racket/trace)

(define (main)
  (output 2 (prime? 2))
  (output 3 (prime? 3))
  (output 4 (prime? 4))
  (output 5 (prime? 5))
  (output 7 (prime? 7))
  (output 42 (prime? 42))
  (output 43 (prime? 43))
  (output 561 (prime? 561))
  (output 1105 (prime? 1105))
  (output 1729 (prime? 1729))
  (output 2465 (prime? 2465))
  (output 2821 (prime? 2821))
  (output 6601 (prime? 6601)))
  
(define (output input result)
  (display "Is ")
  (display input)
  (display " prime? ")
  (if result
    (display "\tyes")
    (display "\tno"))
  (newline))

(define (prime? n)
  ;; Iterate over all possible values for a (1,n-1)
  (define (prime-iter a)
    (define mod-result (fast-expt-mod a (- n 1) n))
    (cond ((= a 0) #t)
	  ((= mod-result 0) #f) ;; Receive the signal that we found a non-trivial square root of 1 mod n
	  ((congruent? mod-result 1 n) (prime-iter (- a 1)))
	  (else #f)))
  (prime-iter (- n 1)))

(define (fast-expt-mod a n m)
  (remainder (expmod-log a n m) m))

(define (expmod-log base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	  (define new-value (remainder (square (expmod-log base (/ exp 2) m)) m))
	  (if (and (!= new-value 1) (!= new-value (- m 1)) (congruent? (square new-value) 1 m))
	    0 ;; Send the signal that we found a non-trivial square root of 1 mod n
	    new-value))
	(else 
	  (remainder (* base (expmod-log base (- exp 1) m)) m))))

;; (trace expmod-log)

(define (!= a b) (not (= a b)))

(define (congruent? a b m)
  (= (remainder (- a b) m) 0))


(define (square x) (* x x))
