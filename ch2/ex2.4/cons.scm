;; NOTE: I actually came up with this before this exercise came up funny enough.

(define (main)
  ;; Example with integers
  (let ((pair (cons2 1 2)))
    (display (car2 pair))
    (newline)
    (display (cdr2 pair))
    (newline))
  ;; Example with points
  (let ((pair (cons2 (make-point 3 4) (make-point 5 6))))
    (print-point (car2 pair))
    (newline)
    (print-point (cdr2 pair))
    (newline)))

(define (cons2 o1 o2)
  (lambda (f) (f o1 o2)))

(define (car2 c)
  (c (lambda (o1 o2) o1)))

(define (cdr2 c)
  (c (lambda (o1 o2) o2)))

;; Points
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")"))
