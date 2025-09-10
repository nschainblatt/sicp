;; a. With Louis's procedures, if both arguments passed to the generic exp procedure are scheme numbers, the correct result is returned. However if two complex numbers are given, an
;;    infinite loop is created because the identity procedures that Louis installed return the same type, leading to apply-generic to fail to find a matching op given the
;;    complex types, leading to it attempting to correct the type again with the same identity procedures.
;;
;; b. Louis is not correct that we need these identity procedures. If the correct procedures exist in the operations table, the correct result will be returned.
;;    If two arguments of the same type are given to the apply-generic procedure, and no appropriate procedure is found, we do not know which direction to try and type cast,
;;    so the appropriate error is thrown to indicate no procedure was found for the types.
;;
;; c. apply-generic does not need to be updated, an error is already thrown if no procedure was found and no coercion procedures were found (because we left our Louis's procedures
;;    that applied coercion to arguments with identical types).

;; NOTE: important for these to be defined first so they overwrite the builtin procedures provided with the std library.
(define (rational? x)
  (equal? (type-tag x) 'rational))
(define (complex? x)
  (equal? (type-tag x) 'complex))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))

(define (main)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  ;; (install-identities-package)
  (install-coercion-package)

  (let ((complex1 (make-complex-from-real-imag 3 5))
        (complex2 (make-complex-from-real-imag 4 9)))

    (println (exp complex2 complex1))))


;; Generics
(define (equ? x y) (apply-generic 'equ x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))


;; Packages
(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (define (complex->scheme-number n)
    (make-scheme-number (car (contents (contents n)))))

  (put 'scheme-number 'complex scheme-number->complex)
  (put 'complex 'scheme-number complex->scheme-number))

(define (install-identities-package)
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (put 'scheme-number 'scheme-number scheme-number->scheme-number)
  (put 'complex 'complex complex->complex))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get type1 type2))
                  (t2->t1 (get type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;; SCHEME-NUMBER
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ '(scheme-number scheme-number) (lambda (x y) (and (number? x) (number? y) (= x y))))
  (put '=zero? '(scheme-number) (lambda (x) (and (number? x) (= x 0))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)

;; RATIONAL
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ '(rational rational) (lambda (x y)
                                   (and (rational? x)
                                        (rational? y)
                                        (= (numer (contents x)) (numer (contents y)))
                                        (= (denom (contents x)) (denom (contents y))))))

  (put '=zero? '(rational) (lambda (x) (and (rational? x)
                                            (= (numer (contents x)) 0))))

  'done)

;; COMPLEX
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (install-complex-package)
  (define real-part car)
  (define imag-part cdr)
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (attach-tag 'rectangular (cons x y)))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex) (lambda (x y) (and (complex? x)
                                                  (complex? y)
                                                  (= (real-part (contents (contents x))) (real-part (contents (contents y))))
                                                  (= (imag-part (contents (contents x))) (imag-part (contents (contents y))))
                                                  (= (magnitude (contents (contents x))) (magnitude (contents (contents y))))
                                                  (= (angle (contents (contents x))) (angle (contents (contents y)))))))

  (put '=zero? '(complex) (lambda (x) (and (complex? x)
                                           (= (real-part (contents (contents x))) 0)
                                           (= (imag-part (contents (contents x))) 0)
                                           (= (magnitude (contents (contents x))) 0)))) ;; NOTE: the angle is undefined
  'done)


(define (apply-specific operation type . args)
  (apply (get operation type) args))

(define operation-type-table '())

;; Retrieve an operations by type in the global generic operations table
(define (get operation type)
  (find-procedure-for-operations operation (find-operations-for-type type operation-type-table)))

;; Place an operation by type in the global generic operations table
(define (put operation type procedure)
  (let ((operations (find-operations-for-type type operation-type-table '())))
    (define (replace-or-append tag item sequence)
      (cond ((null? sequence)  (cons item '())) ;; append to end if not found
            ((equal? tag (type-tag (car sequence)))  (cons item (cdr sequence)))
            (else (cons (car sequence) (replace-or-append tag item (cdr sequence))))))
    (let ((new-operations (replace-or-append operation (cons operation procedure) operations)))
      (let ((new-table (replace-or-append type (append (list type) new-operations) operation-type-table)))
        (set! operation-type-table new-table)))))

(define (find-by-tag tag sequence)
  (cond ((null? sequence) #f)
        ((equal? tag (type-tag (car sequence))) (contents (car sequence)))
        (else (find-by-tag tag (cdr sequence)))))

(define (find-procedure-for-operations operation ops)
  (cond ((null? ops) #f)
        ((equal? operation (type-tag (car ops))) (contents (car ops)))
        (else (find-procedure-for-operations operation (cdr ops)))))

(define (find-operations-for-type type table . default-value)
  (cond ((null? table) (if (null? default-value) #f (car default-value)))
        ((equal? type (type-tag (car table))) (contents (car table)))
        (else (find-operations-for-type type (cdr table) (if (null? default-value) '() (car default-value))))))


;; Tagging
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (square x) (* x x))
