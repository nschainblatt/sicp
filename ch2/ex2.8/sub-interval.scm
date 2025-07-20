(define (main)
  (let ((interval-x (make-interval 6.12 7.48))
        (interval-y (make-interval 6.12 7.48)))
    (display (sub-interval interval-x interval-y))
    (newline)))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; NOTE: the reasoning behind this is our goal for the new lower-bound to be the smallest possible value.
;; and for the new upper-bound to be the biggest.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
