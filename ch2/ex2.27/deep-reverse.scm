(define (main)
  (reverse-list-iter (list (list 1 2) (list 3 4)))
  (reverse-list-iter (list 1 2 3 4 5)))

;; Returns a new list that is the reverse order of my-list.
(define (reverse-list-iter my-list)
  (define (iter sub-list result)
    (if (null? sub-list)
      result
      (let ((head (car sub-list)))
        (iter (cdr sub-list)
              (cons (if (list? head) (reverse-list-iter head) head) result)))))
  (iter my-list '()))

(define (reverse-list-recur my-list)
  (define (recur sub-list)
    (if (null? sub-list)
      '()
      (append (recur (cdr sub-list)) (list (if (list? (car sub-list)) (reverse-list-iter (car sub-list)) (car sub-list))))))
  (recur my-list))
