(define (main)
  (p-t 4))

(define (p-t n) ; note 0 based indexing
  (if (= n 0) 1
      (* (p-t (- n 1)) 2)))

