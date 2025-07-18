(define (main)
  (let ((pair (cons2 1 2)))
    (display (car2 pair))
    (newline)
    (display (cdr2 pair))
    (newline)))

(define (cons2 o1 o2)
  (lambda (f) (f o1 o2)))

(define (car2 c)
  (c (lambda (o1 o2) o1)))

(define (cdr2 c)
  (c (lambda (o1 o2) o2)))
