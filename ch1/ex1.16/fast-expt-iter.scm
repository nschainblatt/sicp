(define (main) 
  (fast-expt 2 16))


(define (square x) (* x x))

(define (even? x)
  (= (remainder x 2) 0))


(define (fast-expt b n) 

  (define counter 0)

  ;; Iterative process
  ;; Time complexity is O(log n)
  ;;   n is divided by constant 2 each time n is even.
  ;;   n is subtracted by a constant 1 each time n is odd, which then the next step n will be even, as are the rules of integers.

  ;; Space complexity is O(1)
  ;;  This procedure is defined to use tail-recursion, which in the mit-scheme interpreter will use
  ;;  TCO (tail call optimization) to make this into a iterative process. This allows the same stack frame
  ;;  (such as local variables b, n, and a) to be reused each subsequent call to fast-expt-iter, instead
  ;;  of new variables being created in a new stack frame each subsequent call.
  (define (fast-expt-iter b n a)
    (set! counter (+ counter 1))
    (if (= n 0)
      a
      (if (even? n)
          (fast-expt-iter (square b) (/ n 2) a)
          (fast-expt-iter b (- n 1) (* a b)))))


  ;; Start
  (display "b^n =")
  (display (fast-expt-iter b n 1))
  (newline)
  (display "Counter: ")
  (display counter)
  (newline))

