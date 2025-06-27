(define (main)
  (gcd 16 28))

(define (gcd a b)
  (if (= b 0) a
    (gcd b (remainder a b))))


; Normal Order Evaluation - Substitution Method
; Each NOTE marks the number of remainders evaluated, need to sum all up to count.
(gcd 206 40)

; Call 1
(define (gcd 206 40)
  (if (= 40 0) 206 
    (gcd 40 (remainder 206 40)))) ; This remainder does not need to be evaluated yet.

; Call 2
(define (gcd 40 (remainder 206 40))
  (if (= (remainder 206 40) 0) 40 ; NOTE: This remainder has to be evaluated because it's required for the if condition. 1
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))) ; But these aren't required, they can be passed to the next call.

; Call 3
(define (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
  ; NOTE: The remainders in this if condition have to be evaluated, but not the value because it doesn't satisfy the condition. 2
  (if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

; Call 3 being evaluated...
(define (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
  (if (= 4 0) (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

; Call 4
(define (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
  ; NOTE: The remainders in this if condition have to be evaluated, but not the value because it doesn't satisfy the condition. 4
  (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

; Call 4 being evaluated...
(define (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
  (if (= 2 0) (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

; Call 5
(define (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
  ; NOTE: The remainders in this if condition have to be evaluated. 7
  ; NOTE: The value also has to be evaluated because the condition is true. 4
  (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))

; Call 5 being evaluated...
(define (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
  (if (= 0 0) 2 ; GCP(206,40) = 2
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))


; NOTE: Total number of remainder operations for normal order evaluation of GCD: 18


; Applicative Order Evaluation - Substitution Method
; Each NOTE marks the number of remainders evaluated, need to sum all up to count.
(gcd 206 40)

; Call 1
(define (gcd 206 40)
  (if (= 40 0) 206 
    (gcd 40 (remainder 206 40)))) ; NOTE: evaluate arguments before passing them to the procedure. 1

; Call 1 being evaluated...
(define (gcd 206 40)
  (if (= 40 0) 206
    (gcd 40 6)))

; Call 2
(define (gcd 40 6)
  (if (= 6 0) 40
    (gcd 6 (remainder 40 6)))) ; NOTE: evaluate arguments before passing them to the procedure. 1

; Call 2 being evaluated...
(define (gcd 40 6)
  (if (= 6 0) 40
    (gcd 6 4)))

; Call 3
(define (gcd 6 4)
  (if (= 4 0) 6
    (gcd 4 (remainder 6 4)))) ; NOTE: evaluate arguments before passing them to the procedure. 1

; Call 3 being evaluated...
(define (gcd 6 4)
  (if (= 4 0) 6
    (gcd 4 2)))

; Call 4
(define (gcd 4 2)
  (if (= 2 0) 4
    (gcd 2 (remainder 4 2)))) ; NOTE: evaluate arguments before passing them to the procedure. 1

; Call 4 being evaluated...
(define (gcd 4 2)
  (if (= 2 0) 4
    (gcd 2 0)))

; Call 5
(define (gcd 2 0)
  (if (= 0 0) 2 ; Base condition reached. 
    (gcd 0 (remainder 2 0)))) ; This is not evaluated as gcd did not get called because of the else.

; NOTE: Total number of remainder operations for applicative order evaluation of GCD: 4
