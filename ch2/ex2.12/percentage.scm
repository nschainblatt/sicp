(define (main)
  (let((i (make-center-percent 50 .1)))
    (display "interval: ")
    (display i)
    (newline)
    (display "center: ")
    (display (center i))
    (newline)
    (display "tolerance: ")
    (display (tolerance i))
    (newline)
    (display "tolerance percent: ")
    (display (tolerance-percent i))
    (newline)))

(define (make-center-percent center-value tolerance-percentage)
  (let ((tolerance (abs (* center-value tolerance-percentage))))
    (make-interval (- center-value tolerance) (+ center-value tolerance))))

(define (tolerance i)
  (abs (/ (- (lower-bound i) (upper-bound i)) 2.0)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (tolerance-percent i)
  (abs (/ (tolerance i) (center i))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))
