(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

;;a. The deriv procedure was updated to utilize the operation-and-type table for all non number and variable expressions (ones that have an
;;   operator and operands). The reason we could not use this approach with the number? and variable? predicates is because the specific
;;   number or variable may vary (e.g. x, y, z or 1, 2, 3). We would have to create a mapping in the table for every possible number or variable
;;   and then make them all point to the same operation shown in the 'deriv' procedure below. It is much simpler to utilize the predicates in
;;   the way shown below then to add all possible combinations to the table.

;;b.
(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define addend car)
  (define (augend operands) (accumulate make-sum 0 (cdr operands)))

  (define multiplier car)
  (define (multiplicand operands) (accumulate make-product 1 (cdr operands)))

  (define base car)
  (define (exponent operands) (accumulate make-exponentiation (cadr operands) (cddr operands)))

  (define (make-exponentiation base exponent)
    (cond ((= exponent 0) 1)
          ((= exponent 1) base)
          (else (list '** base exponent))))

  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (deriv-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))

  (define (deriv-exponentiation operands var)
    (make-product (make-product (exponent operands) (make-exponentiation (base operands) (- (exponent operands) 1)))
                  (deriv (base operands) var)))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation))


;;c. Added exponentiation to the above installation procedure.


;;d. If we switched the positions of the 'deriv and the operator type in the call to get, the changes to the derivitive system
;;   would only require changing the usage of the put procedure in the installation of the package. The actual data operation procedures
;;   would not have to change as the operands and var arguments remain in the same order.
;;  (put '+ 'deriv deriv-sum)
;;  (put '* 'deriv deriv-product)
;;  (put '** 'deriv deriv-exponentiation))
