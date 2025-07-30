(define (main)
  (display (square-list-recur (list 1 2 3 4 5)))
  (newline)
  (display (square-list-iter (list 1 2 3 4 5)))
  (newline)
  (display (square-list-map (list 1 2 3 4 5)))
  (newline))

(define (square-list-recur items)
  (define (square-list-recur sub-items)
    (if (null? sub-items)
      '()
      (cons (square (car sub-items)) (square-list-recur (cdr sub-items)))))
  (square-list-recur items))

(define (square-list-iter items)
  (define (iter sub-items results)
    (if (null? sub-items)
      results 
      (iter (cdr sub-items) (append results (list (square (car sub-items)))))))
  (iter items '()))

(define (square-list-map items)
  (map square items))

(define (square x) (* x x))
