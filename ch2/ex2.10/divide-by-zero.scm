(define (main)
  (let ((interval-x (make-interval 6.12 7.48))
        (interval-y (make-interval 6.12 0)))
    (display (div-interval interval-x interval-y))
    (newline)))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0)) (error "Error: divide by zero")
    (mul-interval x
                  (make-interval (/ 1.0 (lower-bound y))
                                 (/ 1.0 (upper-bound y))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
