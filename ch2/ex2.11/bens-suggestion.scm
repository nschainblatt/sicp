;; Exercise
;; By checking the signs of the bounds of each interval, it is possible to break mul-interval into
;; nine cases, only one of which requires more than two multiplications.

;; My solution
;; We should check the sign of each interval, then use a condition
;; to calculate the new lower and upper bounds based on how we can determine new min and maxes.
;; Use the new lower and upper to return the new interval.

;; All possible combinations (2^4):
;; x + + | y + +
;; x + + | y + -
;; x + + | y - +
;; x + + | y - -
;; x + - | y + +
;; x + - | y + -
;; x + - | y - +
;; x + - | y - -
;; x - + | y + +
;; x - + | y + -
;; x - + | y - +
;; x - + | y - -
;; x - - | y + +
;; x - - | y + -
;; x - - | y - +
;; x - - | y - -

;; However with the rules of lower and upper bound intervals, where the lower is always less than or equal to the upper:
;; x + + | y + +
;; x + + | y + -  -Invalid as if an upper is negative the lower is negative too.
;; x + + | y - +
;; x + + | y - -
;; x + - | y + +  -Invalid as if an upper is negative the lower is negative too.
;; x + - | y + -  -Invalid as if an upper is negative the lower is negative too.
;; x + - | y - +  -Invalid as if an upper is negative the lower is negative too.
;; x + - | y - -  -Invalid as if an upper is negative the lower is negative too.
;; x - + | y + +
;; x - + | y + -  -Invalid as if an upper is negative the lower is negative too.
;; x - + | y - +
;; x - + | y - -
;; x - - | y + +
;; x - - | y + -  -Invalid as if an upper is negative the lower is negative too.
;; x - - | y - +
;; x - - | y - -

;; We are left with the nine cases the problem mentions:
;; x + + | y + +
;; x + + | y - +
;; x + + | y - -
;; x - + | y + +
;; x - + | y - +
;; x - + | y - -
;; x - - | y + +
;; x - - | y - +
;; x - - | y - -

;; Adding the expected lowers and uppers (notice the one extra multiplication):
;; x + + | y + +   -Lower: (* x-lower y-lower) -Upper: (* x-upper y-upper)
;; x + + | y - +   -Lower: (* x-upper y-lower) -Upper: (* x-upper y-upper)
;; x + + | y - -   -Lower: (* x-upper y-lower) -Upper: (* x-lower y-upper)
;; x - + | y + +   -Lower: (* x-lower y-upper) -Upper: (* x-upper y-upper)
;; x - + | y - +   -Lower: (min of products)   -Upper: (* x-upper y-upper)
;; x - + | y - -   -Lower: (* x-upper y-lower) -Upper: (* x-lower y-lower)
;; x - - | y + +   -Lower: (* x-lower y-upper) -Upper: (* x-upper y-lower)
;; x - - | y - +   -Lower: (* x-lower y-upper) -Upper: (* x-lower y-lower)
;; x - - | y - -   -Lower: (* x-upper y-upper) -Upper: (* x-lower y-lower)

(define (main)
  (let ((x++ (make-interval 6.48 7.12))
        (x-+ (make-interval -5.12 8.43))
        (x-- (make-interval -5.12 -2.83))
        (y++ (make-interval 2.52 4.93))
        (y-+ (make-interval -2.52 4.93))
        (y-- (make-interval -2.52 -1.29)))
    (display-interval-test x++ y++)
    (display-interval-test x++ y-+)
    (display-interval-test x++ y--)
    (display-interval-test x-+ y++)
    (display-interval-test x-+ y-+)
    (display-interval-test x-+ y--)
    (display-interval-test x-- y++)
    (display-interval-test x-- y-+)
    (display-interval-test x-- y--)))

(define (display-interval-test interval-x interval-y)
  (display "INTERVALS\n")
  (display interval-x)
  (newline)
  (display interval-y)
  (newline)
  (display "FIRST VERSION\n")
  (display (mul-interval interval-x interval-y))
  (newline)
  (display "SECOND VERSION\n")
  (display (mul-interval-2 interval-x interval-y))
  (newline)
  (newline))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (div-interval x y)
  ;; NOTE: disabling this to check permutations
  ;; (if (spans-zero? y) (error "Error: cannot divide by an interval that spans zero.")
  (mul-interval x
                (make-interval (/ 1.0 (lower-bound y))
                               (/ 1.0 (upper-bound y)))))

(define (spans-zero? x)
  (and (<= (lower-bound x) 0) (>= (upper-bound x) 0)))

(define (<= x y)
  (or (< x y) (= x y)))

(define (>= x y)
  (or (> x y) (= x y)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (mul-interval-2 x y)
  (let ((x-lower (lower-bound x))
        (x-upper (upper-bound x))
        (y-lower (lower-bound y))
        (y-upper (upper-bound y)))

    (cond ((and (both-positive? x-lower x-upper) (both-positive? y-lower y-upper)) (make-interval (* x-lower y-lower) (* x-upper y-upper)))
          ((and (both-positive? x-lower x-upper) (negative? y-lower) (positive? y-upper)) (make-interval (* x-upper y-lower) (* x-upper y-upper)))
          ((and (both-positive? x-lower x-upper) (both-negative? y-lower y-upper)) (make-interval (* x-upper y-lower) (* x-lower y-upper)))
          ((and (negative? x-lower) (positive? x-upper) (both-positive? y-lower y-upper)) (make-interval (* x-lower y-upper) (* x-upper y-upper)))
          ((and (negative? x-lower) (positive? x-upper) (negative? y-lower) (positive? y-upper)) (make-interval (min (* x-lower y-upper) (* x-upper y-lower)) (* x-upper y-upper)))
          ((and (negative? x-lower) (positive? x-upper) (both-negative? y-lower y-upper)) (make-interval (* x-upper y-lower) (* x-lower y-lower)))
          ((and (both-negative? x-lower x-upper) (both-positive? y-lower y-upper)) (make-interval (* x-lower y-upper) (* x-upper y-lower)))
          ((and (both-negative? x-lower x-upper) (negative? y-lower) (positive? y-upper)) (make-interval (* x-lower y-upper) (* x-lower y-lower)))
          ((and (both-negative? x-lower x-upper) (both-negative? y-lower y-upper)) (make-interval (* x-upper y-upper) (* x-lower y-lower))))))

(define (both-positive? x y)
  (and (positive? x) (positive? y)))

(define (both-negative? x y)
  (and (negative? x) (negative? y)))
