(define (main)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-raise-package)
  (install-project-package)
  (install-polynomial-package)
  (let* ((polynomial1 (make-polynomial 'x (make-sparse-term-list (list (list 5 1) (list 0 -1)))))
         (polynomial2 (make-polynomial 'y (make-sparse-term-list (list (list 2 1) (list 0 -1))))))

    (println (add polynomial1 polynomial1))
    (println (add polynomial2 polynomial2))
    (println (add polynomial1 polynomial2))
    (println (mul polynomial1 polynomial2))))

(define exact inexact->exact)

;; Generics
(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

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

(define (div x y)
  (apply-generic 'div x y))

(define (raise n)
  (apply-generic 'raise n))

(define (project n)
  (apply-generic 'project n))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (=zero? x)
  (apply-generic '=zero? x))

(define (negate x)
  (apply-generic 'negate x))

(define (gt x y)
  (apply-generic 'gt x y))

(define (lt x y)
  (apply-generic 'lt x y))

;; --- NUMBER TYPE PACKAGES ---

;; TODO:
;; 1. determine the main variable to convert all other variables to (find the smallest variable by alphabet using string<?)
;; 2. convert any non main variable in the first and second polynomials (and their coefficients recursively) to the main variable (including 0 order terms)
;; 3. make mul ('integer 'polynomial) and ('polynomial 'integer), and the same for addition (may need real too?)
;; 3. with all polynomials converted, perform the desired action (add or multiply)

;; NOTE: remember that any variable in this representation is just a polynomial.

;; NOTE:
;; for mul, multiply the coefficient of each term in the poly by the integer
;; for add, add the integer to the zero order term in the poly, or add the zero order term to the poly (note, may need a way to append to end)
;; for add, add coefficients terms with same order (they have already been converted to the same variable).

;; POLYNOMIALS

;; Constructors
(define (make-polynomial var term-list)
  (apply-specific 'make 'polynomial var term-list))
(define (make-sparse-term-list term-list)
  (attach-tag 'sparse term-list))
(define (make-dense-term-list term-list)
  (attach-tag 'dense term-list))

(define (install-dense-term-list-package)
  (define (tag x) (attach-tag 'dense x))
  (define (make-term order coeff) (list order coeff))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               ;; NOTE: this condition is required as my make-term for dense puts the order information
               ;; inside the term structure, leading to the representation looking more like sparse.
               ;; This is required because the terms must supply the order information in order to not change the implementation
               ;; of the geenric arithmetic procedures for polynomials.
               (if (pair? (car term-list)) 
                 (cadar term-list)
                 (car term-list))))
  (define (rest-terms term-list)
    (tag (cdr term-list)))
  (put 'first-term 'dense first-term)
  (put 'rest-terms 'dense rest-terms))

(define (install-sparse-term-list-package)
  (define (tag x) (attach-tag 'sparse x))
  (define (make-term order coeff) (list order coeff))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list)
    (tag (cdr term-list)))
  (put 'first-term 'sparse first-term)
  (put 'rest-terms 'sparse rest-terms))

(define (install-polynomial-package)
  (install-dense-term-list-package)
  (install-sparse-term-list-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (attach-tag (type-tag term-list) (cons term (contents term-list)))))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  (define (polynomial-zero? p)
    (or (empty-termlist? (term-list p)) (all-coefs-zero? (term-list p))))
  (define (all-coefs-zero? term-list)
    (cond ((null? term-list))
          ((=zero? (coeff (first-term term-list))) (all-coefs-zero? (rest-terms term-list)))
          (else #f)))

  ;; Convert polynomial p to a polynomial with variable var.
  ;; Zero order terms do not remain with the original variable in the coefficient and are instead
  ;; made as zero terms with the new var.
  (define (convert-polynomial-variable p var)
    ;; Loop over every term in p
    ;; If the term is a zero order term, convert directly to var, keep as is for easy adding later
    ;; Otherwise create a new zero order term in var, and make the term a coeffecient with variable p
    ;; polynomial with one term.
    (define (make-term-list sub-p)
      (if (empty-termlist? sub-p)
        (attach-tag (type-tag (term-list p)) (the-empty-termlist))
        (let ((t1 (first-term sub-p)))
          (if (= (order t1) 0)
            (adjoin-term t1 (make-term-list (rest-terms sub-p)))
            (adjoin-term (make-term 0 (tag (make-poly (variable p) (adjoin-term t1 (attach-tag (type-tag (term-list p)) (the-empty-termlist)))))) (rest-terms sub-p))))))
    (make-poly var (make-term-list (term-list p))))

  (define (add-poly p1 p2)
    (let* ((v1 (variable p1))
           (converted-p2 (if (same-variable? v1 (variable p2))
                           p2
                           (convert-polynomial-variable p2 v1))))
      (if (same-variable? (variable p1) (variable converted-p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list converted-p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 converted-p2)))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: SUB-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (let* ((v1 (variable p1))
           (converted-p2 (if (same-variable? v1 (variable p2))
                           p2
                           (convert-polynomial-variable p2 v1))))
      (if (same-variable? (variable p1) (variable converted-p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list converted-p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 converted-p2)))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((gt (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((lt (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L2) L1)
          ((empty-termlist? L1) (negate-terms L2))
          (else (add-terms L1 (negate-terms L2)))))

  (define (mul-terms L1 L2)
    (define (inner L)
      (if (empty-termlist? L)
        (attach-tag (type-tag L1) (the-empty-termlist))
        (add-terms (mul-term-by-all-terms (first-term L) L2)
                   (inner (rest-terms L)))))
    (inner L1))


  (define (mul-term-by-all-terms t1 term-list)
    (define (inner L)
      (if (empty-termlist? L)
        (attach-tag (type-tag term-list) (the-empty-termlist))
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (add (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (inner (rest-terms L))))))
    (if (=zero? (coeff t1))
      (attach-tag (type-tag term-list) (the-empty-termlist))
      (inner term-list)))


  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let* ((quotient-remainder-term-lists (div-terms (term-list p1) (term-list p2)))
             (quotient-term-list (car quotient-remainder-term-lists))
             (remainder-term-list (cadr quotient-remainder-term-lists)))
        (list (make-poly (variable p1) quotient-term-list) (make-poly (variable p1) remainder-term-list)))
      (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (attach-tag (type-tag L1) (the-empty-termlist)) (attach-tag (type-tag L1) (the-empty-termlist)))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (gt (order t2) (order t1))
          (list (attach-tag (type-tag L1) (the-empty-termlist)) L1)
          (let* ((new-c (div (coeff t1) (coeff t2)))
                 (new-o (sub (order t1) (order t2)))
                 (new-term (make-term new-o new-c)))
            (let* ((rest-of-result (mul-term-by-all-terms new-term L2)) ;; multiply the new term by the divisor (L2)
                   (new-dividend (sub-terms L1 rest-of-result))) ;; subtract the result from the dividend (L1), continue recursively until the above case or
              (let* ((quotient-remainder-term-lists (div-terms new-dividend L2))
                     (quotient-term-list (car quotient-remainder-term-lists))
                     (remainder-term-list (cadr quotient-remainder-term-lists)))
                (list (adjoin-term new-term quotient-term-list) remainder-term-list))))))))

  (define (negate-polynomial p)
    (make-poly (variable p) (negate-terms (term-list p))))

  (define (add-poly-int p int)
    ;; If there is a zero order term in p, add int
    ;; Otherwise create zero order term from int and add to end of p's term list
    (define (add-int-to-term-list term-list)
      (let ((t1 (first-term term-list)))
        (cond ((= (order t1) 0) (adjoin-term (make-term 0 (add (coeff t1) int)) (attach-tag (type-tag term-list) (the-empty-termlist))))
              ((empty-termlist? (rest-terms term-list)) (adjoin-term t1 (adjoin-term (make-term 0 int) (attach-tag (type-tag term-list) (the-empty-termlist)))))
              (else (adjoin-term t1 (add-int-to-term-list (rest-terms term-list)))))))
    (make-poly (variable p) (add-int-to-term-list (term-list p))))

  (define (mul-poly-int p int)
    ;; multiply each term in p's term-list by int
    (define (multiply-term-list term-list)
      (if (empty-termlist? term-list)
        (attach-tag (type-tag term-list) (the-empty-termlist))
        (let* ((t1 (first-term term-list)))
          (adjoin-term (make-term (order t1) (mul (coeff t1) int)) (multiply-term-list (rest-terms term-list))))))
    (make-poly (variable p) (multiply-term-list (term-list p))))

  ;; generics
  (define (first-term term-list)
    (apply-specific 'first-term (type-tag term-list) (contents term-list)))
  (define (rest-terms term-list)
    (apply-specific 'rest-terms (type-tag term-list) (contents term-list)))
  (define (empty-termlist? term-list)
    (equal? (contents term-list) (the-empty-termlist)))
  (define (negate-terms term-list)
    (define (inner t)
      (if (empty-termlist? t)
        (attach-tag (type-tag term-list) (the-empty-termlist))
        (let ((term (first-term t)))
          (adjoin-term (make-term (order term) (apply-generic 'negate (coeff term))) (inner (rest-terms t))))))
    (inner term-list))

  ;; selectors
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) '())
  (define (make-term order coeff) (list order coeff))
  (define (tag p) (attach-tag 'polynomial p))

  ;; interface to the rest of the system
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'equ? '(polynomial polynomial) equal?)
  (put '=zero? '(polynomial) polynomial-zero?)
  (put 'negate '(polynomial) (lambda (x) (tag (negate-polynomial x))))
  (put 'sub '(polynomial polynomial) (lambda (x y) (tag (sub-poly x y))))

  (put 'add '(polynomial integer) (lambda (x y) (tag (add-poly-int x y))))
  (put 'add '(integer polynomial) (lambda (x y) (tag (add-poly-int y x))))
  (put 'mul '(polynomial integer) (lambda (x y) (tag (mul-poly-int x y))))
  (put 'mul '(integer polynomial) (lambda (x y) (tag (mul-poly-int y x))))

  'done)



;; INTEGER 
(define (make-integer x)
  ((get 'make 'integer) x))
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (define (add-integer x y) (tag (+ x y)))
  (put 'add '(integer integer) add-integer)
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'equ? '(integer integer) =)
  (put 'square '(integer) (lambda (x) (tag (* x x))))
  (put 'sqrt '(integer) (lambda (x) (tag (sqrt x))))
  (put 'atan '(integer integer) (lambda (x y) (tag (atan x y))))
  (put 'sin '(integer) (lambda (x) (tag (sin x))))
  (put 'cos '(integer) (lambda (x) (tag (sin x))))
  (put '=zero? '(integer) zero?)
  (put 'negate '(integer) (lambda (x) (* x -1)))
  (put 'lt '(integer integer) (lambda (x y) (< x y)))
  (put 'gt '(integer integer) (lambda (x y) (> x y))))


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
  (define (negate-rational x)
    (let ((n (numer x))
          (d (denom x)))
      ;; turn positive to negative
      (cond ((or (and (negative? n) (negative? d)) (and (positive? n) (positive? d))) (make-rat (* (abs n) -1) (abs d)))
            ;; turn negative to positive
            (else (make-rat (abs n) (abs d))))))
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
  (put 'cos '(rational) (lambda (x) (tag (cos-rat x))))
  (put 'negate '(rational) negate-rational))


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
  (put 'cos '(real) (lambda (x) (tag (sin x))))
  (put '=zero? '(real) zero?)
  (put 'negate '(real) (lambda (x) (* x -1))))


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
  (define (negate-complex x)
    (make-from-real-imag (apply-generic 'negate (real-part x)) (apply-generic 'negate (imag-part x))))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex z1 z2 z3))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) complex-equ?)
  (put 'real-part 'complex real-part)
  (put 'imag-part' 'complex imag-part)
  (put 'negate '(complex) negate-complex))


;; Type Ranking System
(define (mydrop n)
  ;; at lowest type, or there are no projection or raise procedures for n
  (if (or (= (tower-rank (type-tag n)) 0) (not (get 'project (type-tag n))) (not (get 'raise (type-tag n))))
    n
    (let* ((projected-arg (project n))
           (reraised-arg (raise projected-arg)))
      (if (equ? n reraised-arg)
        (mydrop projected-arg)
        n))))

(define (tower-rank type)
  (cond ((equal? type 'integer)    0)
        ((equal? type 'rational)   1)
        ((equal? type 'real)       2)
        ((equal? type 'complex)    3)
        ((equal? type 'polynomial) 4)
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


;; OPERATIONS TYPE TABLE
(define operation-type-table '())

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
      (error "Coercion not possible or no proc found for '(args) and 'operation" (map type-tag args) op)
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
    (if (and (not (equal? op 'raise))
             (not (equal? op 'project))
             (not (equal? op 'equ?))
             (not (equal? op '=zero?))
             (not (equal? op 'gt))
             (not (equal? op 'lt)))
      (mydrop (apply proc (map contents args)))
      (apply proc (map contents args))))

  (if (< (length args) 1)
    (error "Must have at least one argument")
    (let* ((type-tags (map type-tag args))
           (proc (get op type-tags)))
      (if proc
        (simplify proc args)
        (coerce-first-matching-type args)))))

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
  (cond ((and (pair? datum) (not (number? (car datum)))) (car datum)) ;; Ensuring type isn't a number to work with polynomial term pairs
        ((integer? datum) 'integer)
        ((real? datum) 'real)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

;; Auxillary
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

