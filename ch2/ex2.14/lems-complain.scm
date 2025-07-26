;; par1 and par2 give two different answers to two equivalent arithmetic expressions because
;; of how we perform the division and multiplication of intervals.

;; During the division and multiplication of intervals, we always create a new interval with new lower and upper bounds.
;; The new lower bound is always the smallest product of all bounds involved and the upper bound is always the largest product of all
;; bounds involved.

;; par1 and par2 have different answers as they perform these order dependent mul/div operations in different quantities and order.

;; For example, the rule of simplifying fractions by cancelling out common factors doesn't work with the division of intervals.
;; In example #1, r1r2/r1 should equal r2, but it doesn't, as the new lower and upper bounds are continually changing to be the smallest
;; and largest values respectively.

;; Another example is #2, dividing a interval by itself should result in an interval with one in both the lower and the upper bounds, but it
;; doesn't for the same reason as example #1.

;; Example #3 proves this even further, during the division of intervals the common pattern for the new lower and upper bounds of the
;; resulting interval seems to always be: New lower-bound = numerator interval lower-bound * reciprocal of the denominator intervals
;; upper-bound. New upper-bound = numerator interval upper-bound * reciprocal of the denominator intervals lower-bound.

(define (main)
  (let ((r1 (make-center-percent 5 .001))
        (r2 (make-center-percent 20 .001))
        (one (make-interval 1 1)))

    ;; Example #1
    (display (div-interval (mul-interval  r1 r2) r1))
    (newline)
    (display (tolerance-percent (div-interval (mul-interval  r1 r2) r1)))
    (newline)
    (display (tolerance (div-interval (mul-interval  r1 r2) r1)))
    (newline)
    (display (center (div-interval (mul-interval  r1 r2) r1)))
    (newline)
    (display r2)
    (newline)
    (newline)

    ;; Example #2
    (display r1)
    (newline)
    (display (div-interval r1 r1))
    (newline)
    (display (tolerance-percent (div-interval r1 r1)))
    (newline)
    (display (center (div-interval r1 r1)))
    (newline)
    (display one)
    (newline)
    (newline)

    ;; Example #3:
    (display (div-interval r1 r2))
    (newline)
    (display (tolerance-percent (div-interval r1 r2)))
    (newline)
    (display (center (div-interval r1 r2)))
    (newline)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (lower-bound y))
                               (/ 1.0 (upper-bound y)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
