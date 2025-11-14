(define (main)
  (let* ((radius 1000)
	 (circle (make-circle (make-point 5 7) radius))
	 (x1 (- (- radius) 5))
	 (x2 (+ radius 5))
	 (y1 (- (- radius) 7))
	 (y2 (+ radius 7))
	 (rectangle-area (* (- x2 x1) (- y2 y1)))
	 (integral (estimate-integral (in-unit-circle? circle) x1 x2 y1 y2))
	 (circle-area (stream-map (lambda (x) (* x rectangle-area 1.0)) integral))
	 (pi-estimate (stream-map (lambda (x) (/ x (square radius))) circle-area)))
    (println (stream-ref pi-estimate 100000))))

(define (estimate-integral P x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (P x y)))

  (define (experiment-stream)
    (cons-stream (experiment) (experiment-stream)))

  (monte-carlo (experiment-stream) 0 0))

(define (in-unit-circle? circle)
  (let ((center (circle-center circle))
	(radius (circle-radius circle)))
    (lambda (x y)
      (<= (+ (square (- x (point-x center))) (square (- y (point-y center)))) (square radius)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
	(stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define (make-circle center radius) (cons center radius))
(define circle-center car)
(define circle-radius cdr)
(define (make-point x y) (cons x y))
(define point-x car)
(define point-y cdr)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x) (* x x))

(define (<= x y)
  (or (< x y) (= x y)))

(define (println x)
  (newline)
  (display x))

(main)
