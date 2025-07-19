(define (main)
  (let ((pair (cons2 1 0)))
    (display (car2 pair))
    (newline)
    (display (cdr2 pair))
    (newline)))

(define (cons2 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car2 c)
  (define (f result counter)
    (if (!= (remainder result 2) 0)
      counter
      (f (/ result 2) (+ counter 1))))
  (f c 0))

(define (cdr2 c)
  (define (f result counter)
    (if (!= (remainder result 3) 0)
      counter
      (f (/ result 3) (+ counter 1))))
  (f c 0))

(define (!= x y)
  (not (= x y)))

(define (expt x n)
  (if (= n 0) 1
    (* x (expt x (- n 1)))))
