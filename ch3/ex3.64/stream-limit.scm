(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (let ((first (stream-car stream))
	(second (stream-car (stream-cdr stream))))
    (if (< (abs (- first second)) tolerance)
      second
      (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map (lambda (guess) (improve-guess guess x)) (sqrt-stream x))))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(newline)
(display (sqrt 25 0.0001))
