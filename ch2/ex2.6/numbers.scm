;; My understanding is that 'zero' returns zero procedures applied to argument x.
;; 'add-1' adds a single procedure 'f' to apply to argument x, with subsequent calls
;; adding additional composed invocations of 'f'.
;; So the number of calls to 'f' is the value of the positive integer we are representing.
;; To represent the number, we can pass procedure 'inc' for parameter 'f' to display an actual number
;; and then for the nested lambda function that accepts parameter 'x' we pass in an initial value of 0.
;; Then based on the number of calls to 'f', we will increment by 1 that many times, displaying the number
;; inside the procedure representation.

(define (main)
  (print-number one)
  (print-number two)
  (print-number (add one two)))

(define (print-number x)
  (display ((x inc) 0))
  (newline))

;; Notice how this definition is similar to the other number definitions (zero, one two)
;; We are returning a new number, and remember that we are representing numbers as procedures.
;; The new number will be the two numbers in the arguments added together, we have already determined
;; that the value of a number in this procedure format is the number of calls to procedure 'f'.
;; So, to get our final number we have to combine each of the arguments calls to 'f'.
;; Ending up with f(f(f(...x))) if 'a' and 'b' were 'one' and 'two' respectively.
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; Notice how we simply add an additional call to 'f' to add one to 'n'.
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc x)
  (+ x 1))

;; Substitution methods

;; (add-1 zero) 1
;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; (lambda (f) (lambda (x) (f x)))
;; (lambda (f) (lambda (x) (f x)))

;; (add-1 (add-1 zero)) 2
;; (add-1 (lambda (f) (lambda (x) (f x))))
;; ((lambda (f) (lambda (x) (f ((n f) x)))) (lambda (f) (lambda (x) (f x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; (lambda (f) (lambda (x) (f (lambda (x) (f x)) x)))
;; (lambda (f) (lambda (x) (f (f x))))
;; (lambda (f) (lambda (x) (f (f x))))
