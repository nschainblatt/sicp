(define (main)
  (let ((set1 '(-100 0 2 3 6 7 8 9 10 100))
        (set2 '(-100 -2 -1 0 1 3 4 5 11 100)))
    (println (union-set set1 set2)))) ;; 1 2 3 4 5 6 7 8 9 10

(define (println x)
  (display x)
  (newline))

(define (adjoin-set x set)
  (cond ((null? set) (list x)) ;; Not in the set, and no item was found greater so add to the end.
        ((= x (car set)) set) ;; Found in set, return original set.
        ((< x (car set)) (cons x set)) ;; Not in the set, found item greater, place between to keep order.
        (else (cons (car set) (adjoin-set  x (cdr set))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; Since both sets are ordered and contain non duplicates, we can iterate through the elements at the same location,
;; compare the values of the numbers, and add the smaller one to the new-set to return. The set whose element is added
;; to the set is cdr'd to go to the next element, while the other set remains the same in the next iteration. If the
;; values in both sets are equal, then one is added to the new-set, and then both sets are cdr'd.
;; With this solution, we don't have to check for existence in the new-set since the sets are ordered and already don't
;; contain duplicates.
;; This solution is linear.
(define (union-set set1 set2)
  (define (iter sub-set-1 sub-set-2 new-set)
    (cond ((or (null? sub-set-1) (null? sub-set-2)) (append new-set sub-set-1 sub-set-2))
          ((> (car sub-set-1) (car sub-set-2)) (iter sub-set-1 (cdr sub-set-2) (append new-set (list (car sub-set-2)))))
          ((< (car sub-set-1) (car sub-set-2)) (iter (cdr sub-set-1) sub-set-2 (append new-set (list (car sub-set-1)))))
          ;; If equal, add one of them to the end of the new set
          (else (iter (cdr sub-set-1) (cdr sub-set-2) (append new-set (list (car sub-set-1)))))))
  (iter set1 set2 '()))
