(define (main)
  (display (last-pair (list 23 72 149 34)))
  (newline)
  (display (last-pair-2 (list 23 72 149 34)))
  (newline))

(define (last-pair l)
  (if (null? (cdr l))
    (list (car l))
    (last-pair (cdr l))))

(define (last-pair-2 l)
  (let ((len (length l)))
    (list (list-ref l (- len 1)))))
