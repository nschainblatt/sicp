(define (main)
  (define table (make-table equal?))

  ;; Insert new key1 sub-table and key2 value
  ((table 'insert) 1 1 'x1)
  ;; Insert new key2 value
  ((table 'insert) 1 2 'x2)
  ((table 'insert) 1 3 'x3)

  ((table 'insert) 2 1 'y1)
  ((table 'insert) 2 2 'y2)
  ((table 'insert) 2 3 'y3)

  ((table 'insert) 3 1 'z1)
  ((table 'insert) 3 2 'z2)
  ((table 'insert) 3 3 'z3)

  ;; Insert replace key2 value
  ((table 'insert) 1 1 'x1.1)
  ((table 'insert) 2 2 'y2.2)
  ((table 'insert) 3 3 'z3.3)

  (table 'print)

  (println ((table 'lookup) 1 1))
  (println ((table 'lookup) 2 2))
  (println ((table 'lookup) 3 3))

  'done)

(define (make-table same-key?)
  (let ((table (list '*table*)))

    (define (insert! key1 key2 value)
      (let ((sub-table (myassoc key1 (cdr table))))
	(if sub-table
	  (let ((record (myassoc key2 (cdr sub-table))))
	    (if record
	      (set-cdr! record value) ;; Replace value.
	      (set-cdr! sub-table (cons (cons key2 value) (cdr sub-table))))) ;; Insert value into sub-table.
	  (set-cdr! table (cons (cons key1 (list (cons key2 value))) (cdr table)))))) ;; Insert new sub-table with value.

    (define (lookup key1 key2)
      (let ((sub-table (myassoc key1 (cdr table))))
	(if sub-table
	  (let ((record (myassoc key2 (cdr sub-table))))
	    (if record
	      (cdr record)
	      #f))
	  #f)))

    ;; Returns the record under the key, or false if none found.
    (define (myassoc key sub-table)
      (cond ((null? sub-table) #f)
	    ((same-key? key (caar sub-table)) (car sub-table))
	    (else (myassoc key (cdr sub-table)))))

    (define (print)
      (define (print-iter sub-table)
	(if (null? sub-table)
	  '()
	  (begin (println (car sub-table)) (print-iter (cdr sub-table)))))
      (print-iter table))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
	    ((eq? m 'lookup) lookup)
	    ((eq? m 'print) (print))
	    (else (error "Invalid operation MAKE-TABLE"))))
    dispatch))


(define (println x)
  (display x) (newline))

(main)
