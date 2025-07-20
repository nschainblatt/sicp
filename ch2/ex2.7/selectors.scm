(define (main)
  (let ((interval (make-interval 6.12 7.48)))
    (display (lower-bound interval))
    (newline)
    (display (upper-bound interval))
    (newline)))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; NOTE: if the order of lower and upper bounds does not matter in the constructor, and the lower-bound
;; is always less than upper-bound (or they are the same), these selector definitions may be better.

(define (lower-bound interval)
  (get-bound interval <))

(define (upper-bound interval)
  (get-bound interval >))

(define (get-bound interval condition)
  (let ((left (car interval))
        (right (cdr interval)))
    (if (condition left right) left right)))
