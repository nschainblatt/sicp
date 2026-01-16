(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Version 1 (with good-enough? and improve are assumed to be primitives)
;; Controller (data-diagram is attached as an image):



;; Version 2 (with good-enough? and improve implemented as arithmetic operations)
;; Controller (data-diagram is attached as an image):
