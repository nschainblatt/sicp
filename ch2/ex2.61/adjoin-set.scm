(define (main)
  (let ((set '(1 2 3 5)))
    (println (adjoin-set 4 set)) ;; 1 2 3 4 5
    (println (adjoin-set 0 set)) ;; 0 1 2 3 5
    (println (adjoin-set 6 set)) ;; 1 2 3 5 6
    (println (adjoin-set 5 set)))) ;; 1 2 3 5

(define (println x)
  (display x)
  (newline))

(define (adjoin-set x set)
  (cond ((null? set) (list x)) ;; Not in the set, and no item was found greater so add to the end.
        ((= x (car set)) set) ;; Found in set, return original set.
        ((< x (car set)) (cons x set)) ;; Not in the set, found item greater, place between to keep order.
        (else (cons (car set) (adjoin-set  x (cdr set))))))
