(define (main)
  (println (fold-right / 1 (list 1 2 3)))      ;; 3/2
  (println (fold-left / 1 (list 1 2 3)))       ;; 1/6
  (println (fold-right list '() (list 1 2 3))) ;; '(1 (2 (3 ())))
  (println (fold-left list '() (list 1 2 3)))  ;; '(((() 1) 2) 3)
  (println (fold-left + 0 (list 1 2 3)))       ;; 6 (same) any commutative operator allows the result to be the same between fold-left and fold-right.
  (println (fold-right + 0 (list 1 2 3)))      ;; 6 (same)
  (println (fold-left * 1 (list 1 2 3)))       ;; 6 (same)
  (println (fold-right * 1 (list 1 2 3))))     ;; 6 (same)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (println x)
  (display x)
  (newline))
