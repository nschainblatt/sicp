;; First representation of rectangles. This uses two segments for the length and height.
;; This however leaves room for error, as the two segments must have a vertex, otherwise the rectangle will be invalid.
;; The second implementation I will use three parameters to the constructor, a vertex point, a length, and a height to correct
;; this room for error.

;; Abstraction Barriers
;; 1. Programs that use rectangles
;; 2. Rectangles in the problem domain: perimeter-rectangle, area-rectangle
;; 3. Rectangles as a length-segment and a height-segment: make-rectangle, length-segment-rectangle, height-segment-rectangle,
;;    length-rectangle, height-rectangle
;; 4. Rectangles as a pair of segments: cons car cdr

(define (main)			                ;; |
				                ;; v Required vertex.
  (let ((length-segment (make-segment (make-point -3 0) (make-point 3 0)))
	(height-segment (make-segment (make-point -3 0) (make-point -3 10))))
    (let ((rectangle (make-rectangle length-segment height-segment)))
      (display rectangle)
      (newline)
      (display (perimeter-rectangle rectangle))
      (newline)
      (display (area-rectangle rectangle))
      (newline))))

(define (make-rectangle length-segment height-segment)
  (cons length-segment height-segment))

(define (length-segment-rectangle rectangle)
  (car rectangle))

(define (height-segment-rectangle rectangle)
  (cdr rectangle))

(define (length-rectangle rectangle)
  (- (x-point (end-segment (length-segment-rectangle rectangle))) (x-point (start-segment (length-segment-rectangle rectangle)))))

(define (height-rectangle rectangle)
  (- (y-point (end-segment (height-segment-rectangle rectangle))) (y-point (start-segment (height-segment-rectangle rectangle)))))

(define (perimeter-rectangle rectangle)
  (let ((length
	  (length-rectangle rectangle))
	(height
	  (height-rectangle rectangle)))
    (+ (* length 2) (* height 2))))

(define (area-rectangle rectangle)
  (let ((length
	  (length-rectangle rectangle))
	(height
	  (height-rectangle rectangle)))
    (* length height)))


;; Segments
(define (make-segment point1 point2)
  (cons point1 point2))

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
