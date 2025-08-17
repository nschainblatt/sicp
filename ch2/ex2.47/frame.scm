(define (main)
  (let ((f1 (make-frame-1 (make-vect 0 0) (make-vect 2 3) (make-vect 5 6)))
        (f2 (make-frame-2 (make-vect 0 0) (make-vect 2 3) (make-vect 5 6))))

    (println f1)
    (println f2)

    (println (get-frame-origin-1 f1))
    (println (get-frame-origin-2 f2))

    (println (get-frame-edge1-1 f1))
    (println (get-frame-edge1-2 f2))

    (println (get-frame-edge2-1 f1))
    (println (get-frame-edge2-2 f2))))


(define (println x)
  (display x)
  (newline))


(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (get-frame-origin-1 frame)
  (car frame))
(define (get-frame-edge1-1 frame)
  (car (cdr frame)))
(define (get-frame-edge2-1 frame)
  (car (cdr (cdr frame))))


(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (get-frame-origin-2 frame)
  (car frame))
(define (get-frame-edge1-2 frame)
  (car (cdr frame)))
(define (get-frame-edge2-2 frame)
  (cdr (cdr frame)))


(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))
