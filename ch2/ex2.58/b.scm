(define (main)
  (deriv '(x + 3 * (x + y + 2)) 'x))

;; NOTE:
;; This solution gives the correct answer to the expression above given from the book.
;; However, using this algorithm on differnt types of expressions and variables doesn't always
;; return the correct differentiation.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))) (deriv (base exp) var)))
        (else
          (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (get-expression-right-of-char '+ s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
  (get-expression-right-of-char '* p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (exponent e)
  (get-expression-right-of-char '** e))

(define (base e)
  (car e))

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else (list base '** exponent))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (get-expression-left-of-char char expression)
  (define (iter sub-expression left-expression)
    (cond 
      ((and (null? sub-expression) (null? char) left-expression))
      ((null? sub-expression) (error "Did not find char in expression"))
      ((eq? (car sub-expression) char) left-expression)
      (else (iter (cdr sub-expression) (append left-expression (list (car sub-expression)))))))
  (let ((left-expression (iter expression '())))
    (if (= (length left-expression) 1)
      (car left-expression)
      left-expression)))

(define (get-expression-right-of-char char expression)
  (cond ((null? expression) (error "Did not find char in expression"))
        ((eq? (car expression) char)
         (if (null? (cdr expression))
           (error "Did not find char in expression")
           (get-expression-left-of-char '() (cdr expression))))
        (else (get-expression-right-of-char char (cdr expression)))))
