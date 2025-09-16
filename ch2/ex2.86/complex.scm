;; Allow any type of number for real, imag, magnitude and angle.
;;   Made generic square, sqrt, atan, sin, and cos procedures as well as specific implementations in each number type package.
;;   In order to add new number types, the above implementations will need to be installed in order for the number package to work correctly.

;; TOWER: integer -> rational -> real

(define exact inexact->exact)

(define (raise n)
  (apply-generic 'raise n))

(define (project n)
  (apply-generic 'project n))

(define (equ? x y)
  (apply-generic 'equ? x y))

;; Repeatedly drop n until base type (integer) or we lose the integrity of our value
(define (mydrop n)
  (if (= (tower-rank (type-tag n)) 0) ;; base type
    n
    (let* ((projected-arg (project n))
           (reraised-arg (raise projected-arg)))
      (if (equ? n reraised-arg) ;; ensure the converted type didn't change value
        (mydrop projected-arg) ;; continue dropping
        n)))) ;; can't drop any further, this is the final value

(define (tower-rank type)
  (cond ((equal? type 'integer)  0)
        ((equal? type 'rational) 1)
        ((equal? type 'real)     2)
        ((equal? type 'complex)  3)
        (else (error "UNSUPPORTED NUMBER TYPE:" type))))

(define (install-raise-package)
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real n)
    (make-real (/ (apply-specific 'numer 'rational n) (apply-specific 'denom 'rational n))))
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  (put 'raise '(integer) integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex))

(define (install-project-package)
  (define (complex->real n)
    (make-real (apply-specific 'real-part 'complex (contents n))))
  (define (real->rational n)
    (make-rational n 1))
  (define (rational->integer n)
    (make-integer (round (/ (apply-specific 'numer 'rational n) (apply-specific 'denom 'rational n)))))
  (put 'project '(complex) complex->real)
  (put 'project '(real) real->rational)
  (put 'project '(rational) rational->integer))

(define (main)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-raise-package)
  (install-project-package)
  (let* ((int1 (make-integer 5))
        (int2 (make-integer 6))
        (rat1 (make-rational 3 9))
        (rat2 (make-rational 1 9))
        (real1 (make-real 0.12345))
        (real2 (make-real 1.12345))
        (complex1 (make-complex-from-real-imag 1 2))
        (complex2 (make-complex-from-mag-ang 6 3))
        (complex-int-rat1 (make-complex-from-real-imag int1 rat2))
        (complex-int-rat2 (make-complex-from-mag-ang int1 rat2)))

    ;; NOTE: operations with complex numbers built with other number types.
    (println (apply-generic 'add complex-int-rat1 complex2 complex1))
    (println (apply-generic 'add complex-int-rat2 complex2 complex1))
    (println (apply-generic 'equ? complex-int-rat2 complex-int-rat1))))


;; --- Auxiliary Procedures ---


;; UPDATED: Multi argument apply-generic to simplify it's answers with mydrop
(define (apply-generic op . args)

  ;; Raises arg of type from-type to to-type or returns false if not possible
  (define (raise-arg-until arg from-type to-type)
    (cond ((> (tower-rank from-type) (tower-rank to-type)) #f)                       ;; arg type higher than to-type (unraisable)
          ((= (tower-rank from-type) (tower-rank to-type)) arg)
          (else
            (let ((raise-arg (get 'raise (list from-type)))) ;; 
              (if raise-arg
                (raise-arg-until (raise-arg (contents arg)) (type-tag (raise-arg (contents arg))) to-type) ;; Raiseable
                #f)))))                                                              ;; No raise procedure found (unraisable)

  (define (coerce-args sub-args t1)
    (define (iter inner-args result)
      (if (null? inner-args)
        result
        (let* ((arg (car inner-args))
               (t2 (type-tag arg))
               (raised-arg (raise-arg-until arg t2 t1)))
          (if raised-arg
            (iter (cdr inner-args) (append result (list raised-arg))) ;; Coerce arg until type of t1 or not possible (false)
            #f)))) ;; Raise not posible
    (iter sub-args '()))

  ;; Converts all outer args to the type of the first arg in sub-args. Returns on first full match. Tries all types in sub-args until first
  ;; match found.
  (define (coerce-first-matching-type sub-args)
    (if (null? sub-args)
      (error "Coercion not possible or no proc found for args" (map type-tag args))
      (let* ((arg (car sub-args))
             (t1 (type-tag arg))
             (coerced-args (coerce-args args t1)))
        (if coerced-args
          (let* ((type-tags (map type-tag coerced-args))
                 (proc (get op type-tags)))
            (if proc
              (simplify proc coerced-args)
              (coerce-first-matching-type (cdr sub-args))))
          (coerce-first-matching-type (cdr sub-args))))))

  (define (simplify proc args)
    (if (and (not (equal? op 'raise)) (not (equal? op 'project)) (not (equal? op 'equ?))) ;; only simplify values, not predicates
      (mydrop (apply proc (map contents args)))
      (apply proc (map contents args))))

  (if (< (length args) 1)
    (error "Must have at least one argument")
    (let* ((type-tags (map type-tag args))
           (proc (get op type-tags)))
      (if proc
        (simplify proc args)
        (coerce-first-matching-type args)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

;; INTEGER 
(define (make-integer x)
  ((get 'make 'integer) x))
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer integer) (lambda (x y z) (tag (+ x y z))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'equ? '(integer integer) =)
  (put 'square '(integer) (lambda (x) (tag (* x x))))
  (put 'sqrt '(integer) (lambda (x) (tag (sqrt x))))
  (put 'atan '(integer integer) (lambda (x y) (tag (atan x y))))
  (put 'sin '(integer) (lambda (x) (tag (sin x))))
  (put 'cos '(integer) (lambda (x) (tag (sin x)))))


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
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (cos-rat x)
    (make-rat (numerator (exact (cos (/ (numer x) (denom x)))))
              (denominator (exact (cos (/ (numer x) (denom x)))))))
  (define (tag x) (attach-tag 'rational x))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'add '(rational rational rational)
       (lambda (x y z) (tag (add-rat (add-rat x y) z))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  (put 'square '(rational) (lambda (x) (tag (make-rat (* (numer x) (numer x)) (* (denom x) (denom x))))))
  (put 'sqrt '(rational) (lambda (x) (tag (make-rat (sqrt (numer x)) (sqrt (denom x))))))
  (put 'atan '(rational rational) (lambda (x y) (tag (make-rat (numerator (exact (atan (/ (numer x) (denom x)) (/ (numer y) (denom y)))))
                                                             (denominator (exact (atan (/ (numer x) (denom x)) (/ (numer y) (denom y)))))))))
  (put 'sin '(rational) (lambda (x) (tag (make-rat (numerator (exact (sin (/ (numer x) (denom x))))) (denominator (exact (sin (/ (numer x) (denom x)))))))))
  (put 'cos '(rational) (lambda (x) (tag (cos-rat x)))))


;; REAL NUMBER
(define (make-real x)
  ((get 'make 'real) x))
(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'add '(real real real) (lambda (x y z) (tag (+ x y z))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'equ? '(real real) =)
  (put 'square '(real) (lambda (x) (tag (* x x))))
  (put 'sqrt '(real) (lambda (x) (tag (sqrt x))))
  (put 'atan '(real real) (lambda (x y) (tag (atan x y))))
  (put 'sin '(real) (lambda (x) (tag (sin x))))
  (put 'cos '(real) (lambda (x) (tag (sin x)))))



(define (square x) 
  (apply-generic 'square x))

(define (mysqrt x)
  (apply-generic 'sqrt x))

(define (myatan x y)
  (apply-generic 'atan x y))

(define (mysin x)
  (apply-generic 'sin x))


(define (mycos x)
  (apply-generic 'cos x))


(define (mul x y)
  (apply-generic 'mul x y))


;; COMPLEX
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (install-complex-package)
  (define real-part car)
  (define imag-part cdr)
  (define (magnitude z)
    (mysqrt (apply-generic 'add (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (myatan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (attach-tag 'rectangular (cons x y)))

  (define (make-from-mag-ang r a)
    (attach-tag 'rectangular (cons (mul r (mycos a)) (mul r (mysin a)))))


  (define (add-complex . args)
    (accumulate (lambda (z1 z2) (make-from-real-imag (apply-generic 'add (real-part (contents z1)) (real-part (contents z2)))
                                                     (apply-generic 'add (imag-part (contents z1)) (imag-part (contents z2)))))
                (make-from-real-imag 0 0)
                args))
  (define (complex-equ? x y)
    (and (apply-generic 'equ? (real-part (contents x)) (real-part (contents y)))
         (apply-generic 'equ? (imag-part (contents x)) (imag-part (contents y)))
         (apply-generic 'equ? (magnitude (contents x)) (magnitude (contents y)))
         (apply-generic 'equ? (angle (contents x)) (angle (contents y)))))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex z1 z2 z3))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) complex-equ?)
  (put 'real-part 'complex real-part)
  (put 'imag-part' 'complex imag-part))


;; OPERATIONS TYPE TABLE
(define operation-type-table '())

(define (apply-specific operation type . args)
  (apply (get operation type) args))

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
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'real)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))
