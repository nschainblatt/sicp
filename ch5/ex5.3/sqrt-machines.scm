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

(controller
  (assign guess (const 1.0))
  test-good-enough?
  (test (op good-enough?) (reg guess))
  (branch (label sqrt-done))
  (assign guess (op improve) (reg guess))
  (goto (label test-good-enough?))
  sqrt-done)


;; Version 2 (with good-enough? and improve implemented as arithmetic operations)
;; Controller (data-diagram is attached as an image):

(controller
  (assign guess (const 1.0))
  test-good-enough?
  ;; Good enough operations
  (assign t (op mul) (reg guess) (reg guess))
  (assign t (op sub) (reg t) (reg x))
  (assign t (op abs) (reg t))
  (test (op <) (reg t) (const 0.001))
  (branch (label sqrt-done))
  ;; Improve operations
  (assign t (op div) (reg x) (reg guess))
  (assign t (op add) (reg guess) (reg t))
  (assign guess (op div) (reg t) (const 2))
  (goto (label test-good-enough?))
  sqrt-done)
