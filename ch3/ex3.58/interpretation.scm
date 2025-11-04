;; Interpretation:

;; The procedure expand creates a stream of the integer and decimal digits involved in dividing 'num' by 'den' in base 'radix'.

;; If you pass a 'num' larger than the 'den', the first number in the stream will be greater than 10. All digits but the last in that
;; first number will be the integer portion of the result. The last digit of the first number and the rest of the numbers in the stream will be the decimal part of the result.

;; If you pass a 'num' less than 'den', the result is just the decimal points.

;; (expand 1 7 10) => (1, 4, 2, 8, 5, 7, same sequence starting from 1 repeating)
;; (expand 3 8 10) => (3, 7, 5, 0, 0, 0, 0 repeating)
;; (expand 29387 4 10) => (73467, 5, 0, 0, 0 repeating) Note you can see that 29387/4 = 7346.75; This confirms the integer portion result above.

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define s1 (expand 1 7 10))

(define s2 (expand 3 8 10))

(define s3 (expand 29387 4 10))

(define (stream-for-each-until proc s ref)
  (cond ((null? s) 'done)
        ((< ref 1) 'done)
        (else (proc (stream-car s)) (stream-for-each-until proc (stream-cdr s) (- ref 1)))))

(define (print x)
  (display x) (display " "))

(newline)
(stream-for-each-until print s1 10)
(newline)
(stream-for-each-until print s2 10)
(newline)
(stream-for-each-until print s3 10)
