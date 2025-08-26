(define (main)
  (let ((set1 '(1 2 2 3 4 4 5))
        (set2 '(1 1 2 2 2 2 3 4 5 6 6 6 7 8 9 10)))
    (println (intersection-set set1 set2))
    (println (union-set set1 set2))))

(define (println x)
  (display x)
  (newline))

;; No change, efficiency is the same
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; We don't check if the element is already in the set, efficiency is now constant instead of linear
(define (adjoin-set x set)
  (cons x set))

;; Even if the set now allows duplicates, the union operation will still need to return a set that has no duplicates because of how
;; union is defined. This was updated to remove the use of adjoin-set since the new version of adjoin-set defined above would allow
;; duplicates. The efficiency of this procedure is quadratic.
(define (union-set set1 set2)
  (define (iter sub-set new-set)
    (cond ((null? sub-set) new-set)
          ((element-of-set? (car sub-set) new-set) (iter (cdr sub-set) new-set))
          (else (iter (cdr sub-set) (cons (car sub-set) new-set)))))
  (iter set1 (iter set2 '())))

;; As with unions, this also returns a set that has no duplicates.
;; The efficiency of this procedure is quadratic.
(define (intersection-set set1 set2)
  (define (iter sub-set2 new-set)
    (cond ((null? sub-set2) new-set)
          ((and (element-of-set? (car sub-set2) set1) (not (element-of-set? (car sub-set2) new-set))) (iter (cdr sub-set2) (cons (car sub-set2) new-set)))
          (else (iter (cdr sub-set2) new-set))))
  (iter set2 '()))
