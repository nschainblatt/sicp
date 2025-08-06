(define (main)
  (println (reverse-right (list 1 2 3 4)))
  (println (reverse-left (list 1 2 3 4))))

(define (reverse-right sequence)
  (fold-right (lambda (curr acc) (append acc (list curr))) '() sequence))

(define (reverse-left sequence)
  (fold-left (lambda (acc curr) (cons curr acc)) '() sequence))

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
