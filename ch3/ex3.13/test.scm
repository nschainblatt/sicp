(define (main)
  (define z (make-cycle (list 'a 'b 'c)))

  (println (last-pair z)) ;; Cycles causes infinite loop attempting to resolve all circular references.

  'done)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (println x)
  (display x) (newline))

(main)
