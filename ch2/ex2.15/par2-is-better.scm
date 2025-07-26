;; Eva Lu Ator is correct in that 'par2' is 'better' because it does not repeat the same interval in it's operations.
;; In par2 r1 and r2 are only operated on once each. While in par1 there operated on twice.
;; Each time we perform arithmentic with an interval we are calculating a new interval with new lower and upper bounds and a new tolerance.
;; Increasing the error bounds. So the fewer operations we have to perform on intervals the tighter error bound we will have.
;; You can see in the examples below that the two algebraically equivelant expressions 'par1' and 'par2' have different error bounds.
;; With 'par2' having a lower tolerance percentage and a more accurate center value because it calculates the equivalent expression with
;; less error prone operations.

(define (main)
  (let ((r1 (make-center-percent 5 .001))
        (r2 (make-center-percent 20 .001)))

    (display "par1")
    (newline)
    (display (par1 r1 r2))
    (newline)
    (display (tolerance-percent (par1 r1 r2)))
    (newline)
    (display (tolerance (par1 r1 r2)))
    (newline)
    (display (center (par1 r1 r2)))
    (newline)
    (newline)

    (display "par2")
    (newline)
    (display (par2 r1 r2))
    (newline)
    (display (tolerance-percent (par2 r1 r2)))
    (newline)
    (display (tolerance (par2 r1 r2)))
    (newline)
    (display (center (par2 r1 r2)))
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
