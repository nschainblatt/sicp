(define (split op1 op2)
  (lambda (painter n)
    (define (inner-split k)
      (if (= k 0)
        painter
        (let ((smaller (inner-split (- k 1))))
          (op1 painter (op2 smaller smaller)))))
    (inner-split n)))

(define (right-split painter n) (split beside below))

(define (up-split painter n) (split below beside))
