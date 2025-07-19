;; My understanding is that 'zero' returns zero procedures applied to argument x.
;; 'add-1' adds a single procedure 'f' to apply to argument x, with subsequent calls
;; adding additional composed invocations of 'f'.
;; So the number of calls to 'f' is the value of the positive integer we are representing.

(define (main)
  (print-number one)
  (print-number two)
  (print-number (add one two)))

(define (print-number x)
  (display ((x inc) 0))
  (newline))

;; Notice how this definition related to 'add-1'.
;; In 'add-1' we only care about adding an additional call to 'f'.
;; In 'add' we are appending to the the number of calls to f that exist
;; in the argument 'a'.
;; This builds: f(f(f(...x)))
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

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
