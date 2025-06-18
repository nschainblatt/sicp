(define (main)
  (fib 5))

; Recursive Process
(define (fib n)
  ; fib n = fib n-1 + fib n-2
  (cond ((< n 1) 0)
	((= n 1) 1)
       (else (+ (fib (- n 1)) (fib (- n 2))))))
