(require racket/trace)

(define counter 0)

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (is-even n)
  (= (remainder n 2) 0))

;; Iterative process
;; Time complexity -> O(log n) this is logarithmic for the same reason as the previous question, the majority of the time the input variabe
;;		      'b' will be even, taking the halving approach which divides the input by 2 each time leading to logarithmic time complexity
;;		      where the total number of operations increases by a constant amount each time the input is doubled.
;; Space complexity -> O(1) constant space because of the iterative process (reusing stack frames each call using TCO)
(define (multi-iter a b p)
  (set! counter (+ counter 1))
  (cond ((= b 0) p)
	(else (if (is-even b)
		  (multi-iter (double a) (halve b) p)
		  (multi-iter a (- b 1) (+ a p))))))

(trace multi-iter)

(define (multi a b)
  (multi-iter a b 0))

(define (main)
  (multi 4 400))
