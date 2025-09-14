;; TOWER: integer -> rational -> real

;; NOTE: raise, project, and equ? procedures are utilized inside mydrop, so they must not use apply-generic otherwise an infinite loop will
;; be created. This is because apply-generic always simplified it's final answers with mydrop, so if mydrop calls apply-generic, and
;; apply-generic calls mydrop, this will lead to ininite circular procedure calls.

(define (raise n)
  (apply-specific 'raise (list (type-tag n)) (contents n)))

(define (project n)
  (apply-specific 'project (list (type-tag n)) (contents n)))

(define (equ? x y)
  (apply-specific 'equ? (list (type-tag x) (type-tag y)) (contents x) (contents y)))

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
        (else (error "UNSUPPORTED NUMBER TYPE"))))

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
  (let ((int1 (make-integer 5))
        (int2 (make-integer 6))
        (rat1 (make-rational 3 9))
        (rat2 (make-rational 1 9))
        (real1 (make-real 0.12345))
        (real2 (make-real 1.12345))
        (complex1 (make-complex-from-real-imag 1 2))
        (complex2 (make-complex-from-real-imag 6 0)))
    ;; All usages of apply-generic will be simplified as far as possible without losing value.
    (println (apply-generic 'add int1 rat1 real1))
    (println (apply-generic 'add rat1 int1 real1))
    (println (apply-generic 'add int1 int1 int1))
    (println (apply-generic 'add int1 int1 rat1))
    (println (apply-generic 'add int1 rat1 complex2))
    (println (project (project (project complex2))))
    (println (mydrop complex2))))


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
      (error "Coercion not possible or no proc found for args" args (map type-tag args))
      (let* ((arg (car sub-args))
             (t1 (type-tag arg))
             (coerced-args (coerce-args args t1)))
        (if coerced-args
          (let* ((type-tags (map type-tag coerced-args))
                 (proc (get op type-tags)))
            (if proc
              (apply proc (map contents coerced-args))
              (coerce-first-matching-type (cdr sub-args))))
          (coerce-first-matching-type (cdr sub-args))))))

  (if (< (length args) 1)
    (error "Must have at least one argument")
    (let* ((type-tags (map type-tag args))
           (proc (get op type-tags)))
      (if proc
        (mydrop (apply proc (map contents args)))
        (mydrop (coerce-first-matching-type args))))))


(define (square x) (* x x))

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
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'equ? '(integer integer) =))


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
  (define (tag x) (attach-tag 'rational x))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'add '(rational rational rational)
       (lambda (x y z) (tag (add-rat (add-rat x y) z))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y))))))


;; REAL NUMBER
(define (make-real x)
  ((get 'make 'real) x))
(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real real) (lambda (x y z) (tag (+ x y z))))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'equ? '(real real) =))


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
  (define (add-complex . args)
    (accumulate (lambda (z1 z2) (make-from-real-imag (+ (real-part (contents z1)) (real-part (contents z2)))
                                                     (+ (imag-part (contents z1)) (imag-part (contents z2)))))
                (make-from-real-imag 0 0)
                args))
  (define (complex-equ? x y)
    (and (= (real-part (contents x)) (real-part (contents y)))
         (= (imag-part (contents x)) (imag-part (contents y)))
         (= (magnitude (contents x)) (magnitude (contents y)))
         (= (angle (contents x)) (angle (contents y)))))


  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex z1 z2 z3))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
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
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))
