(define (main)
  (pt 4 2))

(define (pt r c) ; note 0 based indexing
  (cond ((and (= r 0) (= c 0)) 1)
        ((or (< r 0) (< c 0)) 0)
        (else (+ (pt (- r 1) (- c 1)) (pt (- r 1) c)))))
