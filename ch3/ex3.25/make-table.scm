;; insert! will assume that the last key in the list will be the one to be stored in a new record along with the value.
;; lookup will return record values or sub-tables, depending on the type the last key in the given keys contains.

(define (main)
  (define table (make-table equal?))

  ((table 'insert) (list 1 1) 'x1) ;; ((1 (1 . x1)))
  (table 'print)
  ((table 'insert) (list 1 2) 'x2) ;; ((1 (2 . x2) (1 . x1)))
  (table 'print)
  ((table 'insert) (list 1 3) 'x3) ;; ((1 (3 . x3) (2 . x2) (1 . x1)))
  (table 'print)

  ((table 'insert) (list 2 1) 'y1)
  (table 'print)
  ((table 'insert) (list 2 2) 'y2)
  ((table 'insert) (list 2 3) 'y3) ;; ((2 (3 . y3) (2 . y2) (1 . y1)) (1 (3 . x3) (2 . x2) (1 . x1)))

  ((table 'insert) (list 3 1) 'z1)
  ((table 'insert) (list 3 2) 'z2)
  ((table 'insert) (list 3 3) 'z3) ;; ((3 (3 . z3) (2 . z2) (1 . z1)) (2 (3 . y3) (2 . y2) (1 . y1)) (1 (3 . x3) (2 . x2) (1 . x1)))

  ((table 'insert) (list 1 1) 'x1.1)
  ((table 'insert) (list 2 2) 'y2.2)
  ((table 'insert) (list 3 3) 'z3.3)
  ;; ((3 (3 . z3.3) (2 . z2) (1 . z1)) (2 (3 . y3) (2 . y2.2) (1 . y1)) (1 (3 . x3) (2 . x2) (1 . x1.1)))

  (table 'print)

  ((table 'insert) (list 10 9 8 7 6 5 4 3 2 1) 'TEST)
  ((table 'insert) (list 10 9 8 7 6 5 4 3 2 1) 'TESTAGAIN)

  (println ((table 'lookup) (list 10 9 8 7 6 5 4 3 2 1))) ;; Returns the record value 'TESTAGAIN
  (println ((table 'lookup) (list 10 9 8 7 6 5 4 3 2))) ;; Returns the sub-table holding the record with value 'TESTAGAIN

  'done)

(define (make-table same-key?)
  (let ((table (list '*table*)))

    ;; Note that an unexpected error could happen if keys were given in the wrong order, such as if you gave 3 keys, and the middle
    ;; key was to a record and not another sub-table.
    (define (insert! keys value)

      ;; Inserts the remaining keys as sub-tables in table, until the last which will be stored with the value as a record.
      (define (insert-remaining-keys sub-keys prev-table)
	(cond ((null? sub-keys) 'done) ;; This wouldn't happen unless sub-keys was originally null passed in
	      ((null? (cdr sub-keys))
	       ;; car is the last key, so set to the new record instead of a table.
	       (set-cdr! prev-table (cons (cons (car sub-keys) value) (cdr prev-table)))
	       'done)
	      (else
		;; Insert a new table, and proceed to the next key
		(let ((new-table (cons (car sub-keys) '())))
		  (set-cdr! prev-table (cons new-table (cdr prev-table)))
		  (insert-remaining-keys (cdr sub-keys) new-table)))))

      (define (iter sub-keys sub-table)
	(cond ((null? sub-keys)
	       ;; Ran out of keys, which means we've reached our target record. Sub table is the record since assoc returns the car of the pair which is the record instead of another table here.
	       (set-cdr! sub-table value))
	      ;; More keys and table to traverse
	      (else
		(let ((next-table (myassoc (car sub-keys) (cdr sub-table))))
		  (if next-table
		    (iter (cdr sub-keys) next-table)
		    (insert-remaining-keys sub-keys sub-table))))))

      (iter keys table))

    ;; Returns the value in a record or sub-table located at the keys path.
    (define (lookup keys)
      (define (iter sub-keys sub-table)
	(if (null? sub-keys)
	  (if (pair? (cdr sub-table)) sub-table (cdr sub-table))
	  (let ((next-item (myassoc (car sub-keys) (cdr sub-table))))
	    (if next-item
	      (iter (cdr sub-keys) next-item)
	      #f))))
      (iter keys table))

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
	    ((eq? m 'print) (println table))
	    (else (error "Invalid operation MAKE-TABLE"))))
    dispatch))


(define (println x)
  (display x) (newline))

(main)
