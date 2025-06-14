; Iterative Process

(define (main)
  (f 4))

; keep track of three f(n)'s (because they are required to make the next f(n)) -> f(3) = f(2) + 2(f(1)) + 3(f(0)).
; use n as a counter, lowering until it is less than three because we start out given the first three starting f(n)'s.
; 2 1 0 represent the 3 starting points because f(3) is the first greater than the base case.

(define (f n)
  (if (< n 3) n
      (f-iter 2 1 0 n)))

(define (f-iter n1 n2 n3 count)
  (if (< count 3) n1
    (f-iter (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (- count 1))))
