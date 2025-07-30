(define (main)
  (display (same-parity-iter 1 2 3 4 5 6 7))
  (newline)
  (display (same-parity-iter 2 3 4 5 6 7))
  (newline)
  (display (same-parity-iter 2))
  (newline)
  (display (same-parity-recur 1 2 3 4 5 6 7))
  (newline)
  (display (same-parity-recur 2 3 4 5 6 7))
  (newline)
  (display (same-parity-recur 2))
  (newline))

;; Iterative process, except the results are reversed.
(define (same-parity-iter x . y)
  (let ((parity (remainder x 2)))
    (define (iter sub-y same-parity-list)
      (cond ((null? sub-y) same-parity-list)
            ((= parity (remainder (car sub-y) 2)) (iter (cdr sub-y) (cons (car sub-y) same-parity-list)))
            (else (iter (cdr sub-y) same-parity-list))))
    (iter y (list x))))

;; Iterative process in the correct order. Except I have to wrap each subsequent value in the y-list in a new list in order to use
;; the append method, keeping the same order.
(define (same-parity-iter x . y)
  (let ((parity (remainder x 2)))
    (define (iter sub-y same-parity-list)
      (cond ((null? sub-y) same-parity-list)
            ((= parity (remainder (car sub-y) 2)) (iter (cdr sub-y) (append-single-element same-parity-list (car sub-y))))
            (else (iter (cdr sub-y) same-parity-list))))
    (iter y (list x))))

(define (append-single-element my-list element)
  (append my-list (list element)))

;; Recursive process in the correct order.
(define (same-parity-recur x . y)
  (let ((parity (remainder x 2)))
    (define (recur sub-y)
      (cond ((null? sub-y) '())
            ((= parity (remainder (car sub-y) 2)) (cons (car sub-y) (recur (cdr sub-y))))
            (else (recur (cdr sub-y)))))
    (cons x (recur y))))
