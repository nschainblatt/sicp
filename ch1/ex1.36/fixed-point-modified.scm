;; Modify the 'fixed-point' procedure to print the sequence of approximations it makes.
;; Find a solution to x^x = 1000 using the 'fixed-point' procedure with x |-> log(1000)/log(x).
;; Compare the number of steps with and without average damping, (when using without will have to use a guess greater than 1).
 ;; NOTE: cannot use first-guess of 1, will cause division by zero.

(define steps 0)

(define (main)
  (println "Without average:")
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1)
  (println steps)
  (set! steps 0)
  (println "With average:")
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
  (println steps))


(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (try guess)
      (let ((next (f guess)))
	 (set! steps (+ steps 1))
	 (println next)
         (if (good-enough? guess next tolerance)
	    next
	    (try next))))
    (try first-guess))

(define (good-enough? x y tolerance)
  (< (abs (- x y)) tolerance))

(define (average x y)
  (/ (+ x y) 2))

(define (println x)
  (display x)
  (newline))
