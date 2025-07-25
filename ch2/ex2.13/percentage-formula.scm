;; Show that there is a formula for finding the percentage tolerance of the product of two intervals.
;; The formula uses the percentage tolerance of each of the factors.

;; I am not sure exactly what the question is asking, because we could easily discover the accurate percentage
;; tolerance by simply computing the product, and then getting the tolerance percentage from the result.
;; I did notice however, that when using two intervals with extremly small tolerance percentages that the
;; percentage of the product is found simply by adding the two percentages together.



(define (main)
  (let((i1 (make-center-percent 500 .0007))
       (i2 (make-center-percent 15 .0007)))
    (display "interval 1: ")
    (display i1)
    (display "\tpercentage tolerance: ")
    (display (tolerance-percent i1))
    (display "\ttolerance: ")
    (display (tolerance i1))
    (newline)
    (display "interval 2: ")
    (display i2)
    (display "\tpercentage tolerance: ")
    (display (tolerance-percent i2))
    (display "\ttolerance: ")
    (display (tolerance i2))
    (newline)
    (let ((i-product (mul-interval i1 i2)))
      (display "i-product: ")
      (display i-product)
      (display "\tpercentage tolerance: ")
      (display (tolerance-percent i-product))
      (display "\ttolerance: ")
      (display (tolerance i-product))
      (newline)
      (display "i1 and i2 tolerage percentages added together: ")
      (display (+ (tolerance-percent i1) (tolerance-percent i2)))
      (newline))))

(define (make-center-percent center-value tolerance-percentage)
  (let ((tolerance (abs (* center-value tolerance-percentage))))
    (make-interval (- center-value tolerance) (+ center-value tolerance))))

(define (tolerance i)
  (abs (/ (- (lower-bound i) (upper-bound i)) 2.0)))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (average x y)
  (/ (+ x y) 2))

(define (tolerance-percent i)
  (abs (/ (tolerance i) (center i))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
