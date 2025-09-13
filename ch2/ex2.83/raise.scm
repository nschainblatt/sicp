;; TOWER: integer -> rational -> real


;; For each type (except complex), design a procedure that raises objects of that type one level in the tower. 
(define (install-coercions-package)
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real n)
    (make-real (/ (apply-specific 'numer 'rational n) (apply-specific 'denom 'rational n))))
  (put 'raise '(integer) integer->rational)
  (put 'raise '(rational) rational->real))


;; Show how to install a generic raise operation that will work for each type (ex- cept complex).
(define (raise n)
  (apply-generic 'raise n))


(define (main)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-coercions-package)
  (let ((int1 (make-integer 5)))
    (raise (raise int1))))


;; --- Auxiliary Procedures ---


;; Multi argument apply-generic
(define (apply-generic op . args)
  ;; Converts all sub-args to t1 or returns false if not possible (no coercion procedure found for types)
  (define (coerce-args sub-args t1)
    (define (iter inner-args result)
      (if (null? inner-args)
        result
        (let* ((arg (car inner-args))
               (t2 (type-tag arg))
               (t2->t1 (get t2 t1)))
          (cond ((equal? t1 t2) (iter (cdr inner-args) (append result (list arg))))  ;; same type, no need to coerce, add to built list
                (t2->t1 (iter (cdr inner-args) (append result (list (t2->t1 arg))))) ;; coercion exists, convert and add to built list
                (else #f)))))
    (iter sub-args '()))                                               ;; No type coercion found
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
  (put 'make 'integer (lambda (x) (tag x))))


;; RATIONAL
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-rational-package)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom))


;; REAL NUMBER
(define (make-real x)
  ((get 'make 'real) x))
(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
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
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'integer)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))
