(define the-empty-table '(head))
(define (make-table)
  (let ((table the-empty-table))

    (define (scan type sub-table)
      (cond ((null? sub-table) #f)
	    ((eq? type (caar sub-table)) (car sub-table))
	    (else (scan type (cdr sub-table)))))

    (define (lookup type proc)
      (let ((type-result (scan type (cdr table))))
	(if type-result
	  (let ((proc-result (scan proc (cdr type-result))))
	    (if proc-result
	      (cdr proc-result)
	      #f))
	  #f)))

    (define (insert type proc operation)
      (let ((type-result (scan type (cdr table))))
	(if type-result
	  (let ((proc-result (scan proc (cdr type-result))))
	    (println proc-result)
	    (if proc-result
	      (set-cdr! proc-result operation)
	      (set-cdr! type-result (cons (cons proc operation) (cdr type-result)))
	      ))
	    (set-cdr! table (cons (list type (cons proc operation)) (cdr table))))))

    (define (print)
      (println table))

    (define (dispatch message)
      (cond ((eq? message 'lookup) lookup)
	    ((eq? message 'insert) insert)
	    ((eq? message 'print)  print)
	    (else (error "unknown message --make-table"))))
    dispatch))

(define eval-procedure-table (make-table))

(define (get type proc)
  ((eval-procedure-table 'lookup) type proc))

(define (put type proc operation)
  ((eval-procedure-table 'insert) type proc operation))

(define (print)
  ((eval-procedure-table 'print)))
