;; 1. Write procedure 'width-interval'.
;; 2. Utilize the new procedure with an interval created by addition or subtraction.
;; 3. Utilize the new procudure with an interval created by multiplication or division.
;; 4. Show examples why the width procedure doesn't work the same when the created interval was done by multiplication or division.

;; Find the width of two intervals before adding and then the width after adding.
;; (interval-x (make-interval 6.12 7.48))
;; Original width: 0.68
;; (interval-y (make-interval 6.12 7.48))
;; Original width: 0.68
;; (add-interval interval-x interval-y)
;; New width: 1.36
;; Answer You can see that the new width is indeed a function of the two argument intervals (adding them together).

;; Find the width of two intervals before mutliplying and then the width after multiplying.
;; (interval-x (make-interval 6.12 7.48))
;; Original width: 0.68
;; (interval-y (make-interval 6.12 7.48))
;; Original width: 0.68
;; (mul-interval interval-x interval-y)
;; New width: 9.248
;; Answer: You can see that the new width is not a function of the two argument intervals, you cannot
;; make the new width using the widths of the original two intervals.

;; With addition/subtraction, the width increases or decreases by a combination of the original two widths.

;; With multiplication/division, the width behaves differently as the new lower and upper bounds
;; are being multiplied by the other intervals bounds to obtain a new set of lower and upper bounds.
;; The width of the new interval is completely unrelated to the original widths.

(define (main)
  (let ((interval-x (make-interval 6.12 7.48))
        (interval-y (make-interval 6.12 7.48)))
    (display (width-interval interval-x))
    (newline)
    (display (width-interval (add-interval interval-x interval-y)))
    (newline)
    (display (width-interval (sub-interval interval-x interval-y)))
    (newline)
    (display (width-interval (mul-interval interval-x interval-y)))
    (newline)
    (display (width-interval (div-interval interval-x interval-y)))
    (newline)))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

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

;; The width is a measure of the uncertainty of the number specified by the interval.
;; Which means the tolerance, room for error.
(define (width-interval x)
  (/ (abs (- (lower-bound x) (upper-bound x))) 2))
