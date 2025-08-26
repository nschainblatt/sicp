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
  (define (iter pre-set post-set)
    (cond ((null? post-set) (append pre-set (list x))) ;; Not in the set, and no item was found greater so add to the end.
          ((= x (car post-set)) (append pre-set post-set)) ;; Found in set, return original set.
          ((< x (car post-set)) (append pre-set (list x) post-set)) ;; Not in the set, found item greater, place between to keep order.
          (else (iter (append pre-set (list (car post-set))) (cdr post-set)))))
  (iter '() set))
