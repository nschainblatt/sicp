;; Utilize 'cont-fract-iter' to approximate e based on Euler's expansion.

(require racket/trace)

(define (main)
  (+ 2 (cont-fract-iter (lambda (i) 1.0)
	           d
	           10)))

(define (d i)
  (cond ((= i 0) 2)
	((= (remainder (+ i 1) 3) 0) (* (/ (+ i 1) 3) 2))
	(else 1)))

;; In an iterative process, we don't have any delayed operations.
;; So to build an iterative process for a k-term finite continued fraction, we have to start
;; at the bottom and build upwards (backwards) vs. how we did in the recursive process, we started
;; at the top and worked our way down, using delayed operations to complete the fraction.

(define (cont-fract-iter n d k)
  (define (iter i result)
    (if (= i 1)
      (/ (n i) result)
      (iter (- i 1) (+ (d (- i 1)) (/ (n i) result)))))
  (iter k (d k)))
