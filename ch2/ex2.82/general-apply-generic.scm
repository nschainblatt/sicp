(require racket/trace)
;; The reason the original apply-generic with coercion isn't considered sufficiently general because we will prioritize the first coercion match.
;; This is inside the cond, where if the first coercion procedure we check has a value, we always apply that one. Which may lead to always failing to find a procedure because
;; we don't retry the other types in that cond.
;; For example, if t1->t2 contains a procedure instead of #f, the first cond condition will always evaluate, leaving the second procedure in the cond to never be attempted.
;; (let ((t1->t2 (get type1 type2))
;;       (t2->t1 (get type2 type1)))
;;   (cond (t1->t2
;;           (apply-generic op (t1->t2 a1) a2))
;;         (t2->t1
;;           (apply-generic op a1 (t2->t1 a2)))
;;         (else (error "No method for these types"
;;                      (list op type-tags))))))

;; The new 'generalized' general-apply-generic isn't sufficiently general either mostly because of the same reason. We did add more types to coerce, but each time we do we eliminate,
;; other potential types to try. For example, with the first call, we find a coercion procedure to apply, and call general-apply-generic again with the coerced types, however we also
;; left behind another type coercion possibility in the remaining cond conditions.

(define (rational? x)
  (equal? (type-tag x) 'rational))
(define (complex? x)
  (equal? (type-tag x) 'complex))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (add . args)
  (apply general-apply-generic (append (list 'add) args)))

(trace add)

(define (main)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-coercion-package)

  (let ((complex1 (make-complex-from-real-imag 3 5))
        (complex2 (make-complex-from-real-imag 4 9)))

    (println complex1)
    (println (add 6 complex2))))



(define nil '())

;; Original
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

;; TODO:
;; - when no proc is found, attempt to coerce all args (if possible) one type at a time, starting with the first
;; - when all args can coerce to the first, then we will try to find an op with those types (this is the reason its not very general, first match will cause other type coercions to
;;     note be attempted)
;; NOTE:
;; Updates include the following:
;;   - if a procedure isn't found for the 'op' and type of 'args', then we try to coerce all arguments to the type of the first argument until the last, stopping on the first
;;     match of updated types. When a match is found, we try again with those types with general-apply-specific. If no match is found we throw an error like before.
;;     This version will only try the first match once, and then since all future calls to general-apply-generic will have all arguments of the same type, if no proc is found then
;;    then a error is thrown.
;;
;;    TODO: test this, but also probably need to update to actually try the other types to coerce, instead of trying the first match, then probably failing immediately after because
;;          all the types are the same on the next call to general-apply-specfic leading to an error.
;;          NOTE: could update the entire procedure, meaning make general-apply-generic remember the original args so we can try again with a new type if no proc was found
;;                after the coercion. vvvvv
;;  
;; Updated 'generalized' version
(define (general-apply-generic op . args)
  (println (list "debug args" args))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((all-args-same-type? args (type-tag (car args))) (error "No proc found, no coercions possible all arguments are of the same type" args))
            (else (general-apply-generic op (find-first-args-coercion args)))))))

(define (find-first-args-coercion args)
  (define (recur sub-args)
    (cond ((null? sub-args) (error "No method for these types" (list args)))
          ((all-args-can-coerce? args (type-tag (car sub-args))) (coerce-all args (type-tag (car sub-args)))) ;; NOTE: important for args and subargs here.
          (else (recur (cdr sub-args)))))
  (recur args))


(define (all-args-can-coerce? args type)
  (if (null? args)
    #t
    (let ((t1->t2 (get type (type-tag (car args)))))
      (if (or t1->t2 (equal? type (type-tag (car args)))) ;; procedure exists or same type
        (all-args-can-coerce? (cdr args) type)
        #f))))

;; TODO: LEFT OFF HERE, FOR SOME REASON A LIST WRAPS THE FINAL ANSWER, NEED TO RESTART ANYWAYS BECAUSE WE DON'T TRY ALL TYPE COERCIONS. JUST START COMPLETELY FRESH.
(define (coerce-all args type)
  (if (null? args)
    nil
    (let* ((arg (car args))
           (t1->t2 (get (type-tag arg) type)))
      (println (list "debug" arg))
      (if (equal? type (type-tag arg))
        (cons arg (coerce-all (cdr args) type)) ;; type is the same, don't coerce (we don't have identities anymore)
        (cons (t1->t2 arg) (coerce-all (cdr args) type))))))

;; (trace coerce-all)

(define (all-args-same-type? args type)
  (cond ((null? args) #t)
        ((not (equal? type (type-tag (car args)))) #f)
        (else (all-args-same-type? (cdr args) type))))


;; Generics
(define (equ? x y) (apply-generic 'equ x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))


;; Packages
(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (define (complex->scheme-number n)
    (make-scheme-number (car (contents (contents n)))))

  (trace complex->scheme-number)

  (put 'scheme-number 'complex scheme-number->complex)
  (put 'complex 'scheme-number complex->scheme-number))


;; SCHEME-NUMBER
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
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
    (make-from-real-imag (+ (real-part (contents z1)) (real-part (contents z2)))
                         (+ (imag-part (contents z1)) (imag-part (contents z2)))))
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
