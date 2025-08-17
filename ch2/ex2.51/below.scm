#lang racket
(require sicp-pict)

(define (main)
  (paint (below-2 einstein (escher))))


(define (below-1 painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top (transform-painter painter2 split-point  (make-vect 1 0.5) (make-vect 0 1)))
          (paint-bottom (transform-painter painter1 (make-vect 0 0) (make-vect 1 0) split-point )))
      (lambda (frame) (paint-top frame) (paint-bottom frame)))))


(define (below-2 painter1 painter2)
  (180-counter (270-counter (beside (270-counter painter1) (270-counter painter2)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.0)
              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (flip-horiz painter)
  (transform-painter painter (make-vect 1 0) (make-vect 0 0) (make-vect 1 1)))
(define (180-counter painter)
  (transform-painter painter (make-vect 1 1) (make-vect 0 1) (make-vect 1 0)))
(define (270-counter painter)
  (transform-painter painter (make-vect 0 1) (make-vect 0 0) (make-vect 1 1)))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (vector-sub (m corner1) new-origin)
                   (vector-sub (m corner2) new-origin)))))))


(define (println x)
  (display x)
  (newline))


(main)
