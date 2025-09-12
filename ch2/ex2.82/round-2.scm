;; The reason the original two argument version of apply-generic is not sufficiently general is because
;; if both the coercions of t1->t2 and  t2->t1 exist, only t1->t2 will every be tried. This is because t1->t2 will be truthy in the cond,
;; leading to another call to apply-generic with the two arguments of type t2, then, because we don't have identity coercions added in the table
;; the procedure will end with an error because no proc or coercion procs were found.

;; With my updated 'generalized' apply-generic procedure. I do feel it is sufficiently general because we try to find the proc for all matching
;; coercions.
;; Although, an argument can be made that since I use the first matching coerced args with a matching proc, it may not be the best choice
;; (maybe another coercion would be more accurate). This is because coercions are attempted in the order of the arguments, so if a the
;; first argument has a not-as-accurate type, but all other args can be coerced to it and there exists a proc for these args, then we will
;; use that one because we don't know about the accuracy.

;; Original two argument version
;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;         (apply proc (map contents args))
;;         (if (= (length args) 2)
;;           (let ((type1 (car type-tags))
;;                 (type2 (cadr type-tags))
;;                 (a1 (car args))
;;                 (a2 (cadr args)))
;;             (let ((t1->t2 (get type1 type2))
;;                   (t2->t1 (get type2 type1)))
;;               (cond (t1->t2
;;                       (apply-generic op (t1->t2 a1) a2))
;;                     (t2->t1
;;                       (apply-generic op a1 (t2->t1 a2)))
;;                     (else (error "No method for these types"
;;                                  (list op type-tags))))))
;;           (error "No method for these types"
;;                  (list op type-tags)))))))


(define (add . args)
  (apply apply-generic (append (list 'add) args)))

(define (main)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-coercion-package)

  (let ((complex1 (make-complex-from-real-imag 3 5))
        (complex2 (make-complex-from-real-imag 4 9))
        (rat1 (make-rational 1 2))
        (rat2 (make-rational 3 8)))

    ;; This will attempt to turn all arguments to rationals first, but no coercions exist (we didn't install any), so then we try to turn
    ;; all arguments to the next arg type (complex), that works, so then we look up a 'add procedure for 5 complex numbers, if it exists GREAT, if not then we try to coerce all arguments to scheme-numbers, same thing repeats.
    ;; In this case we will add all numbers as complex because that coercion and add proc exist.
    (println (add rat1 complex2 rat2 8 7))))

;; My multiple argument version
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
      (error "Complete coercion not possible, no coercion procedure applicable to all arguments:" args)
      (let* ((arg (car sub-args))
             (t1 (type-tag arg))
             (coerced-args (coerce-args args t1)))
        (if coerced-args
          (let ((type-tags (map type-tag coerced-args)))
            (let ((proc (get op type-tags)))
              (if proc
                (apply proc (map contents coerced-args))
                (coerce-first-matching-type (cdr sub-args)))))
          (coerce-first-matching-type (cdr sub-args))))))

  (if (< (length args) 1)
    (error "Must have at least one argument")
    (coerce-first-matching-type args)))

;; Packages
(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (define (complex->scheme-number n)
    (make-scheme-number (car (contents (contents n)))))

  (define (rational->scheme-number n)
    (make-scheme-number (/ (car (contents n)) (cdr (contents n)))))

  (define (complex->rational n)
    (make-complex-from-real-imag (/ (car (contents n)) (cdr (contents n))) 0))

  (put 'scheme-number 'complex scheme-number->complex)
  (put 'complex 'scheme-number complex->scheme-number)
  (put 'rational 'scheme-number rational->scheme-number)
  (put 'rational 'complex complex->rational))


;; SCHEME-NUMBER
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number scheme-number scheme-number scheme-number) (lambda args (tag (accumulate + 0 args))))
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
  (define (add-complex . args)
    (accumulate (lambda (z1 z2) (make-from-real-imag (+ (real-part (contents z1)) (real-part (contents z2)))
                                                     (+ (imag-part (contents z1)) (imag-part (contents z2)))))
                (make-from-real-imag 0 0)
                args))
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
  (put 'add '(complex complex complex complex complex)
       (lambda (z1 z2 z3 z4 z5) (tag (add-complex z1 z2 z3 z4 z5))))
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


;; OPERATIONS TYPE TABLE
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


;; HELPERS
(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))
