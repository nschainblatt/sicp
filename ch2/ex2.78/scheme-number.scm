(define (main)
  (install-scheme-number-package)
  (let ((num1 (apply-specific 'make 'scheme-number 5))
        (num2 (apply-specific 'make 'scheme-number 6)))
    (println (apply-specific 'mul '(scheme-number scheme-number) (contents num1) (contents num2)))))

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
  'done)

;; ---

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
  (cond ((null? ops) (error "operation not found in operations" operation ops))
        ((equal? operation (type-tag (car ops))) (contents (car ops)))
        (else (find-procedure-for-operations operation (cdr ops)))))

(define (find-operations-for-type type table . default-value)
  (cond ((null? table) (if (null? default-value) (error "operations not found for type" type) (car default-value)))
        ((equal? type (type-tag (car table))) (contents (car table)))
        (else (find-operations-for-type type (cdr table) (if (null? default-value) '() (car default-value))))))

;; Modifications made to tag procedures to allow the scheme-number package to utilize primitives instead of pairs with tag 'scheme-number.
;; No change was required for the scheme-number package.
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))
