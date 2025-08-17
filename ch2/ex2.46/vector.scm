(define (main)
  (let ((v1 (make-vect 2 3))
        (v2 (make-vect 5 6)))
    (println (add-vect v1 v2))
    (println (sub-vect v1 v2))
    (println (scale-vect v1 3))))

(define (println x)
  (display x)
  (newline))

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
