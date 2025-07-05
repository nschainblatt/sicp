(require racket/trace)

(define (main)
  (display (product even? identity 1 inc 10))
  (newline)
  (display (product odd? identity 1 inc 10))
  (newline)
  (display (sum even? identity 1 inc 10))
  (newline)
  (display (sum odd? identity 1 inc 10))
  (newline)
  (display (squared-prime-numbers-sum 1 100))
  (newline)
  (display (relative-prime-product 10))
  (newline))

;; Create a 'filtered-accumulate' procedure that takes the same arguments as 'accumulate', but
;; it also takes a 'predicate' procudure as an additional argument that will be used to indicate
;; whether the current value of 'a' should be combined with the current result.

;; Write 'filtered-accumulate' to have a recursive process.
(define (filtered-accumulate combiner predicate null-value term a next b)
  (if (> a b)
        null-value
	(combiner
          (if (predicate a) (term a) null-value)
          (filtered-accumulate combiner predicate null-value term (next a) next b))))

;; Write 'filtered-accumulate-iter' to have a iterative process.
(define (filtered-accumulate-iter combiner predicate null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((predicate a)
	    (iter (next a) (combiner (term a) result)))
	  (else (iter (next a) result))))
  (iter a null-value))

;; Using the 'filtered-accumulate' procedure, sum the square of the prime numbers in range a-b
(define (squared-prime-numbers-sum a b)
  (sum prime? square a inc b))

;; Using the 'filtered-accumulate' procedure, calculate the product of all positive integers less
;; than 'n' that are relatively prime to 'n' (the GCD of the current integer and 'n' equals 1.)

(define (relative-prime-product n)
  (define (predicate i)
    (relatively-prime? i n))
  (product predicate identity 1 inc (- n 1)))

(define (sum predicate term a next b)
  (filtered-accumulate + predicate 0 term a next b))

(define (product predicate term a next b)
  (filtered-accumulate * predicate 1.0 term a next b))

(define (prime? n)
  (define (prime-iter a)
    (define mod-result (fast-expt-mod a (- n 1) n))
    (cond ((= a 0) #t)
	  ((= mod-result 0) #f)
	  ((congruent? mod-result 1 n) (prime-iter (- a 1)))
	  (else #f)))
  (if (< n 2) #f
    (prime-iter (- n 1))))

(define (fast-expt-mod a n m)
  (remainder (expmod-log a n m) m))

(define (expmod-log base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	  (define new-value (remainder (square (expmod-log base (/ exp 2) m)) m))
	  (if (and (!= new-value 1) (!= new-value (- m 1)) (congruent? (square new-value) 1 m))
	    0
	    new-value))
	(else 
	  (remainder (* base (expmod-log base (- exp 1) m)) m))))

(define (congruent? a b m)
  (= (remainder (- a b) m) 0))

(define (!= a b) (not (= a b)))
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (not (even? x)))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (relatively-prime? i n) (= (gcd i n) 1))
