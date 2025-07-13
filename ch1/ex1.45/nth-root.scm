;; Figure out how many times we have to average damp when computing nth roots.
;; Utilize fixed-point, average-damp, and repeated procedures to implement the found pattern.

(define (main)
  (display (nth-root 2 1)) (newline)
  (display (nth-root 4 2)) (newline)
  (display (nth-root 8 3)) (newline)
  (display (nth-root 16 4)) (newline)
  (display (nth-root 32 5)) (newline)
  (display (nth-root 64 6)) (newline)
  (display (nth-root 128 7)) (newline)
  (display (nth-root 256 8)) (newline)
  (display (nth-root 512 9)) (newline)
  (display (nth-root 1028 10)) (newline)
  (display (nth-root 2056 11)) (newline))

;; NOTES:
;; 2 requires 1 average damp
;; 3 requires 1 average damp
;; 4 requires 2 average damp
;; 5 requires 2 average damp
;; 6 requires 2 average damp
;; 7 requires 2 average damp
;; 8 requires 3 average damp
;; Looks like we have to average damp by the power of 2.
;; For 2 (2^1) we do 1 damp, 4 (2^2) we do 2 damps, 8 (2^3) we do 3 damps.

(define (nth-root x n)
  (if (< n 1)
    (error "nth-root only accepts positive values of n.")
    (cond ((= n 1) x)
          (else
            (define (recur i)
                (if (>= i n)
                  (average-damp (lambda (guess) (/ x (fast-expt guess (- n 1)))))
                  (average-damp (recur (* i 2)))))
            (fixed-point (recur 2) 1.0)))))

(define (>= x y)
  (or (> x y) (= x y)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (fast-expt b n) 
  (define (fast-expt-iter b n a)
    (if (= n 0)
      a
      (if (even? n)
          (fast-expt-iter (square b) (/ n 2) a)
          (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

(define (square x) (* x x))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (good-enough? x y)
        (< (abs (- x y)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
         (if (good-enough? guess next)
	    next
	    (try next))))
    (try first-guess))
