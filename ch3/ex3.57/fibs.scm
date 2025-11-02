(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define add
  (let ((count 0))
    (lambda (x y)
      (set! count (+ count 1))
      (newline) (display count) ;; Last number printed is 3, n-1 additions.
      (+ x y))))

(define (add-streams s1 s2)
  (stream-map add s1 s2))

(newline)
(display (stream-ref fibs 4)) ;; Note that this is the 4th Fibonacci number even though stream-ref is 0 index based, the first Fibonacci
                              ;; number is 1, not 0.

;; a.
;; Procedure fibs performs n-1 additions for the nth Fibonacci number. This is because the first Fibonacci number
;; 1 has already been computed. For any additional Fibonacci numbers, one addition operation will be required.
;; This has linear time complexity.

;; b.
;; The number of additions would exponentially increase without memoization because each subsequent Fibonacci number to be calculated would
;; have to recompute all previous Fibonacci numbers, including all addition operations.
;; This has exponential time complexity.
