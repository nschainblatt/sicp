(define (main)
  (test1)
  (test2)
  (test3)
  (test4))

;; Positive numerator and denominator produce a positive rational number.
(define (test1)
  (let ((rat (make-rat 4 8)))
	(assert-equals 1 (numer rat))
	(assert-equals 2 (denom rat)))
  (display ""))

;; Negative numerator and positive denominator produce a negative rational number.
(define (test2)
  (let ((rat (make-rat -3 24)))
	(assert-equals -1 (numer rat))
	(assert-equals 8 (denom rat)))
  (display ""))

;; Positive numerator and negative denominator produce a negative rational number.
(define (test3)
  (let ((rat (make-rat 4 -16)))
	(assert-equals -1 (numer rat))
	(assert-equals 4 (denom rat)))
  (display ""))

;; Negative numerator and denominator produce a positive rational number.
(define (test4)
  (let ((rat (make-rat -10 -10)))
	(assert-equals 1 (numer rat))
	(assert-equals 1 (denom rat)))
  (display ""))

(define (assert-equals expected actual)
  (assert (= expected actual) (cons expected (cons "didnt equal" actual))))

(define (assert val message)
  (if val #t
    (println message)))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (println (denom rat)))

(define (println val)
  (display val)
  (newline))

(define (make-rat n d)
  (let ((g (gcd n d))
	(sign (if (< d 0) -1 1)))
    (cons (* (/ n g) sign) (* (/ d g) sign))))

(define (xor x y)
  (not (= x y)))

(define (is-neg x)
  (< x 0))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))
