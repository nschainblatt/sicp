;; Second implementation of rectangles. This uses a vertex point, length, and height internally to represent the rectangle.
;; In many game engines, they typically choose a corner to begin drawing from, such as coordinate (0, 0), as well as the direction
;; they draw, for example up and right.
;; I am doing something similar: the vertex-point is assumed to be the bottom left corner of the rectangle.
;; The height and length go up and right respectively from the vertex.

;; In this second representation of rectangles, the perimeter and area procedures didn't change. Just the representation
;; of the rectangle (constructor, length-rectangle, height-rectangle).

;; Abstraction Barriers
;; 1. Programs that use rectangles
;; 2. Rectangles in the problem domain: perimeter-rectangle, area-rectangle <-- These didn't change from the first representation
;; 3. Rectangles as a vertex, length, and height: make-rectangle, length-rectangle, height-rectangle, vertex-rectangle <-- Everything here
;;    and below did change
;; 4. Rectangles as a pair of plairs: cons car cdr

(define (main)
  (let ((vertex-point (make-point -5 0)))
    (let ((rectangle (make-rectangle vertex-point 10 5)))
      (display rectangle)
      (newline)
      (display (perimeter-rectangle rectangle))
      (newline)
      (display (area-rectangle rectangle))
      (newline))))

(define (make-rectangle vertex-point length height)
  (cons vertex-point (cons length height)))

(define (length-rectangle rectangle)
  (car (cdr rectangle)))

(define (height-rectangle rectangle)
  (cdr (cdr rectangle)))

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
