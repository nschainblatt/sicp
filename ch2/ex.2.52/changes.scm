#lang racket

(require sicp-pict)

;; a. Add smile to wave
(define (wave)
  (segments->painter (list (make-segment (make-vect 0.2 0) (make-vect 0.3 0.5))
                           (make-segment (make-vect 0.3 0) (make-vect 0.4 0.3))
                           (make-segment (make-vect 0.5 0) (make-vect 0.4 0.3))
                           (make-segment (make-vect 0.6 0) (make-vect 0.5 0.5))

                           (make-segment (make-vect 0.7 0.2) (make-vect 0.5 0.5))
                           (make-segment (make-vect 0 0.6) (make-vect 0.1 0.4))
                           (make-segment (make-vect 0.1 0.4) (make-vect 0.2 0.6))
                           (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.5))

                           (make-segment (make-vect 0 0.8) (make-vect 0.1 0.6))
                           (make-segment (make-vect 0.1 0.6) (make-vect 0.2 0.65))
                           (make-segment (make-vect 0.2 0.65) (make-vect 0.3 0.65))
                           (make-segment (make-vect 0.3 0.65) (make-vect 0.2 0.8))

                           (make-segment (make-vect 0.2 0.8) (make-vect 0.3 0.9))
                           (make-segment (make-vect 0.5 0.9) (make-vect 0.6 0.8))
                           (make-segment (make-vect 0.6 0.8) (make-vect 0.5 0.65))
                           (make-segment (make-vect 0.5 0.65) (make-vect 0.6 0.65))
                           (make-segment (make-vect 0.6 0.65) (make-vect 0.7 0.4))

                           (make-segment (make-vect 0.3 0.75) (make-vect 0.5 0.75))
                           (make-segment (make-vect 0.3 0.75) (make-vect 0.35 0.7))
                           (make-segment (make-vect 0.5 0.75) (make-vect 0.45 0.7))
                           (make-segment (make-vect 0.45 0.7) (make-vect 0.35 0.7)))))


;; b.  Modify corner-split
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner))))))


(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))


(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0 0)
              split-point
              (make-vect 0 1)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1 0)
              (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
            (transform-painter
              painter1
              (make-vect 0 0)
              (make-vect 1 0.5)
              split-point))
          (paint-top
            (transform-painter
              painter2
              split-point
              (make-vect 1 0.5)
              (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (vector-sub (m corner1) new-origin)
                   (vector-sub (m corner2) new-origin)))))))


;; c. Modify square-limit (point inwards instead of outwards)
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)))

(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 1 1)))

(define (rotate180 painter)
  (transform-painter
   painter
   (make-vect 1 1)
   (make-vect 0 1)
   (make-vect 1 0)))

(define (identity x) x)


(paint (square-limit einstein 2))
