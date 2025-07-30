(define (main)
  (foreach (lambda (x) (display x) (newline)) (list 1 2 3 4 5)))

(define (foreach proc my-list)
  (define (iter sub-list)
    (cond ((null? sub-list) '())
          (else 
            (proc (car sub-list))
            (iter (cdr sub-list)))))
  (iter my-list))
