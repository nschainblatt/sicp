(define (main)
  (my-equal? 'a '(a)))

(define (my-equal? obj1 obj2)
  (cond ((and (pair? obj1) (pair? obj2))  (and (my-equal? (car obj1) (car obj2)) (my-equal? (cdr obj1) (cdr obj2))))
        (else (eq? obj1 obj2))))
