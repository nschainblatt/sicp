;; Equivalent algebraic expressions may lead to different answers because of the change in the error tolerance between arithmentic
;; operations. As you could see in the r1/r1 example, the answer is not a new interval which the lower and upper bounds equal 1, with a 
;; tolerance of 0 and a center of 1. The result contains an error tolerance even for the most trivial arithmetic expression. So with that
;; mind, the more complex the expressions and the more operations applied to intervals in that expression, the higher amount of error tolerance
;; there will be.o

;; Ideas for an interval-arithmetic package that doesn't not have these error tolerance issues:

;;  - Detect when dividing a number by itself. Return the interval 'one'.

;;  - Implement algebraic rules into the operation procedures themselves
;;    (such as cancelling out common factors in the numerator and denominator)

;;  - Perhaps use a different form of evaluation to first attempt to simplify the algebraic expression to it's simplest form, and then solve.
;;    This would differ from the 'normal operations' order the scheme interpreter already uses for arithmetic, since we are performing 
;;    arithmetic with a custom data structure, we would have to implement the specification.
;;    This would require 'interpreting' the expressions, locating patterns, and simplifying to a common form.

;;  - On top of the evaluation option above, we could simplify to a expression that contains the least possible amount of interval operations.
;;    This way we have the least amount of error tolerance in the result.
;;    Such how 'par2' is algebraically equivalent to 'par1', however 'par2' produces a result with less error tolerance as it has less
;;    repeated interval operations.

;; - So perhaps this 'simplifying' operation could itself be a higher order function that would take the operators and the operands
;;   involved and perform the simplification.

;; - I can see the simplification process being simpler than the process to optimize the expression to use the least amount of interval
;;   operations.

;; - In the example of 'par2' you can simplify this expression to equal 'par2' by removing the fraction in the denominator. However this
;;   simplification makes 'par2' less optimal.

;; I think an interval-arithmetic package that solves these issues is very possible. I wish to revisit this after I explore better methods
;; for combining data and learning how the interpreter works.

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
