(define (main)
  (let ((segment (make-segment (make-point 1 1) (make-point -7 -5))))
    (print-segment segment)
    (display "midpoint: ")
    (print-point (midpoint-segment segment))
    (newline)))


;; Segments
(define (make-segment point1 point2)
  (cons point1 point2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point start) (x-point end)) (average (y-point start) (y-point end)))))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-segment segment)
  (display "segment: ")
  (print-point (start-segment segment))
  (display " ")
  (print-point (end-segment segment))
  (newline))


;; Points
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")"))


;; Helpers
(define (average c1 c2)
  (/ (+ c1 c2) 2))
