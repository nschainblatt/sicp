;; Utilize 'cont-fract-iter' to approximate tangent in radians.

(define (main)
  (tan-cf 5 10))

(define (tan-cf x k)
  (define (n-tangent i)
    (if (= i 1)
       x
        ;; NOTE: Reversing the sign here to not modify the my original 'cont-fract-iter' procedure.
	;; This is because the tangent continued fraction requires (n-tangent - d-tangent).
	;; So we are doing (d-tangent + -n-tangent).
       (* x x -1.0)))
  (define (d-tangent i)
    (+ i (- i 1)))
  (cont-fract-iter n-tangent d-tangent k))


(define (cont-fract-iter n d k)
  (define (iter i result)
    (if (= i 1)
      (/ (n i) result)
                     ;; NOTE: this is where (d-tangent + -n-tangent) occurs.
      (iter (- i 1) (+ (d (- i 1)) (/ (n i) result)))))
  (iter k (d k)))
