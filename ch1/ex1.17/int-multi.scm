(define counter 0)

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (is-even n)
  (= (remainder n 2) 0))

;; Recursive process
;; Time complexity O(log n) -> Dividing b (base case input) by a constant each call when b is even,
;;   subtracting 1 from b when b is odd, making it even the next time around. Most multi calls are 
;;   going to have an even 'b', so this is why it is logarithmic.
;; Space complexity O(log n) -> Same reason as time complexity.
(define (multi a b)
  (set! counter (+ counter 1))
  (cond ((= b 0) 0)
	((= b 1) a)
	(else (if (is-even b)
		  (multi (double a) (halve b))
		  (+ a (multi a (- b 1)))))))

(define (main)
  (display (multi 4 100))
  (newline)
  (display counter)
  (newline))
