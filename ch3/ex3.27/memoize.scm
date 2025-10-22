;; Explain why memo-fib computes the nth Fibonacci number in a number of steps proportional to n.
;;  The reason why memo-fib computes the nth Fibonacci number in a number of steps proportional to n is because
;; we eliminate the duplicate calculations that were being made before. With the memoization changes, we only have to compute
;; a fibonacci number for a distinct number once, leading to linear time complexity.

;; Would the scheme still work if we had simply defined memo-fib to be (memoize fib)?
;;  No, the behavior would be different and we would be doing duplicate calculations again. The reason is because in the cond
;; of fib, the procedure fib is called instead of memo-fib. This means that each fib call after the first would not be utilizing
;; the memoization features of the cache table within memoize.

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
	      (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))

(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
	    ((= n 1) 1)
	    (else (+ (memo-fib (- n 1))
		     (memo-fib (- n 2))))))))
