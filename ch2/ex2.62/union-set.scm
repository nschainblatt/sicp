(define (main)
  (let ((set1 '(1 2 3 4 5 6 7 8 9 10))
        (set2 '(1 2 3 5)))
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

(define (union-set set1 set2)
  (accumulate adjoin-set set1 set2))
