(define (main)
  (display "Examples using 'Accumulate'\n")
  ;; Sum integers a-b
  (display (accumulate + 0 identity 1 inc 10))
  (newline)
  ;; Product of integers a-b (Factorial b)
  (display (accumulate * 1 identity 1.0 inc 10))
  (newline)
  ;; Approximation of PI.
  (display (* 4 (accumulate * 1 wallis-term 1.0 inc 1000)))
  (newline)

  (display "\nExamples using 'Sum and Product'\n")
  ;; Sum integers a-b
  (display (sum identity 1 inc 10))
  (newline)
  ;; Product of integers a-b (Factorial b)
  (display (product identity 1 inc 10))
  (newline)
  ;; Approximation of PI.
  (display (* 4 (product wallis-term 1 inc 1000)))
  (newline))
  
;; Show how product and sum procedures can be defined as simple calls to accumulate.
(define (product term a next b)
  (accumulate * 1.0 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;; Write 'accumulate' to have a recursive process.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
   (combiner
     (term a)
     (accumulate combiner null-value term (next a) next b))))

;; Write 'accumulate-iter' to have a iterative process.
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
    (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (identity x) x)

(define (inc x ) (+ x 1))

(define (even? x) (= (remainder x 2) 0))

(define (wallis-term a)
  (cond ((even? a) (/ (+ a 2) (+ a 1)))
        (else (/ (+ a 1) (+ a 2)))))
