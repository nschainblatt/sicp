;; TOWER: integer -> rational -> real

;; NOTE: to add new types to the system, all that is required is to add the type to the tower ranking system, create the coercion procedures,
;; as well as an installation package for the new type. No existing packages and neither apply-generic will have to be updated. It is all
;; based around the tower ranking system.

(define (tower-rank type)
  (cond ((equal? type 'integer)  0)
        ((equal? type 'rational) 1)
        ((equal? type 'real)     2)
        (else (error "UNSUPPORTED NUMBER TYPE"))))

(define (install-coercions-package)
  (define (integer->rational n)
    (make-rational (contents n) 1))
  (define (rational->real n)
    (make-real (/ (apply-specific 'numer 'rational (contents n)) (apply-specific 'denom 'rational (contents n)))))
  (put 'raise 'integer integer->rational)
  (put 'raise 'rational rational->real))


(define (raise n)
  (apply-generic 'raise n))


(define (main)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-coercions-package)
  (let ((int1 (make-integer 5))
        (rat1 (make-rational 2 9))
        (real1 (make-real 0.12345)))
    (println (apply-generic 'add int1 rat1 real1)) ;; converts all to real
    (println (apply-generic 'add rat1 int1 real1)) ;; same, different order
    (println (apply-generic 'add int1 int1 int1))  ;; leaves all as int
    (println (apply-generic 'add int1 int1 rat1))));; converts all to rational


;; --- Auxiliary Procedures ---


;; UPDATED: Multi argument apply-generic
(define (apply-generic op . args)

  ;; Raises arg of type from-type to to-type or returns false if not possible
  (define (raise-arg-until arg from-type to-type)
    (cond ((> (tower-rank from-type) (tower-rank to-type)) #f)                       ;; arg type higher than to-type (unraisable)
          ((= (tower-rank from-type) (tower-rank to-type)) arg)
          (else
            (let ((raise-arg (get 'raise from-type)))
              (if raise-arg
                (raise-arg-until (raise-arg arg) (type-tag (raise-arg arg)) to-type) ;; Raiseable
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
      (error "Coercion not possible or no proc found for args" args)
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
        (apply proc (map contents args))
        (coerce-first-matching-type args)))))


;; INTEGER 
(define (make-integer x)
  ((get 'make 'integer) x))
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer integer) (lambda (x y z) (tag (+ x y z))))
  (put 'make 'integer (lambda (x) (tag x))))


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
       (lambda (n d) (tag (make-rat n d)))))


;; REAL NUMBER
(define (make-real x)
  ((get 'make 'real) x))
(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real real) (lambda (x y z) (tag (+ x y z))))
  (put 'make 'real (lambda (x) (tag x))))


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
