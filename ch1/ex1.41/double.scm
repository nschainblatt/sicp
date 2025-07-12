;; Produces 4 nested calls to double with inc as the procedure argument.
;; This results in 2^4 calls to inc, resulting in: 5 + 16 = 21.

(define (main)
  (((double (double double)) inc) 5))

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

;; (((double (double double)) inc) 5)
;; (((double (lambda (x) (double (double x)))) inc) 5)
;; (((lambda (x) (double (double (double (double x))))) inc) 5)
;; ((double (double (double (double inc)))) 5)
;; ((double (double (double (lambda (x) (inc (inc x)))))) 5)
;; ((double (double (lambda (x) ((inc (inc (inc (inc x)))))))) 5)
;; ((double (((inc (inc (inc (inc (inc (inc (inc (inc x))))))))))) 5)
;; (((((inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))))) 5)
