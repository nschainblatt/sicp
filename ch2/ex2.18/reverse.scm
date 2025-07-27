(require racket/trace)

(define (main)
  (display (reverse-list (list 1 2 3 4 5 6 7 8 9 10)))
  (newline)
  (display (reverse-list (list 1)))
  (newline)
  (display (reverse-list (list)))
  (newline))

(define (reverse-list my-list)
  (define (rev-iter sub-list reversed-list)
    (if (null? sub-list)
      reversed-list
      (rev-iter (cdr sub-list) (cons (car sub-list) reversed-list))))
  (rev-iter my-list '()))
