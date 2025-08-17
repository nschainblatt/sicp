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
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

(define (make-vect x y)
  (cons x y))
