(define (main)
  (define acc (make-accumulator 0))
  (println (acc 10))  ;; 10
  (println (acc 20))) ;; 30

(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))
