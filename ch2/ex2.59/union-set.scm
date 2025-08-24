(define (main)
  (union-set '(1 2 3 4 5) '(1 2 3 4 5 6 7 8 9 10)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

;; The union of set1 and set2 is all elements of set1, plus all the elements of set2 that are not in set1.
(define (union-set set1 set2)
  (define (recur sub-set2)
    (cond ((null? sub-set2) '())
          ((element-of-set? (car sub-set2) set1) (recur (cdr sub-set2)))
          (else (cons (car sub-set2) (recur (cdr sub-set2))))))
  (append set1 (recur set2)))
