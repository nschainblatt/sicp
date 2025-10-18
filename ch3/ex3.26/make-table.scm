;; TODO:
;; 1. Update the table and sub-table representation to be a binary tree instead of just a list of records.
;;    - We will need a representation for a tree as well as selectors and a constructor.
;;    - We will need a procedure to create a tree from a list, to then use a procedure to turn a list into a balanced tree.
;; 2. Update insert! to insert the keys in order from smallest to biggest (numerically or string comparison).
;;    - Whenever we create a new table, we need to create a binary tree instead of just a list.
;;    - Update the iter inside of insert! to traverse the binary tree and sub-table trees.
;;    - Update the insert-remaining-keys procedure to insert the remining keys as additional sub-tables in binary tree
;;      representation.
;; 3. Update lookup to perform a binary tree search for the value under the keys given.
;;    - Create a new iter inside of lookup to traverse each tree and sub-tree of the subsequent tables.


;; NOTE: new insert! flow:
;; 1. Traverse the main tables contents (is a tree) looking for the next key in keys, going through all the keys until the end.
;;    - If along the way we don't find a node in the current sub-tree to continue searching through, we must create new sub-tables
;;      as tree nested all the way down until the final node we create that will hold the value to insert.
;;    - If we make it through all the keys, simply update the final nodes value to the new value to insert.

;; NOTE: new lookup flow:
;; 1. TODO:

(define (main)
  (define table (make-table equal?))

  ((table 'insert) (list 1 1) 'x1)
  (table 'print)
  ((table 'insert) (list 1 2) 'x2)
  (table 'print)
  ((table 'insert) (list 1 3) 'x3)
  (table 'print)

  ((table 'insert) (list 2 1) 'y1)
  (table 'print)
  ((table 'insert) (list 2 2) 'y2)
  ((table 'insert) (list 2 3) 'y3)

  ((table 'insert) (list 3 1) 'z1)
  ((table 'insert) (list 3 2) 'z2)
  ((table 'insert) (list 3 3) 'z3)

  ((table 'insert) (list 1 1) 'x1.1)
  ((table 'insert) (list 2 2) 'y2.2)
  ((table 'insert) (list 3 3) 'z3.3)

  (table 'print)

  ((table 'insert) (list 10 9 8 7 6 5 4 3 2 1) 'TEST)
  ((table 'insert) (list 10 9 8 7 6 5 4 3 2 1) 'TESTAGAIN)

  (println ((table 'lookup) (list 10 9 8 7 6 5 4 3 2 1)))
  (println ((table 'lookup) (list 10 9 8 7 6 5 4 3 2)))

  'done)

(define (insert! keys value table)
  ((table 'insert) keys value))

(define (lookup keys table)
  ((table 'lookup) keys))

(define (make-table same-key?)

  (define (make-record key value) (cons key value))
  (define (record-key record) (car record))
  (define (record-value record) (cdr record))
  (define (set-record-key! record key) (set-car! record key))
  (define (set-record-value! record value) (set-cdr! record value))
  (define (table? potential-table) (pair? (cdr potential-table)))

  (let ((table (list '*table*)))

    ;; Note that an unexpected error could happen if keys were given in the wrong order, such as if you gave 3 keys, and the middle
    ;; key was to a record and not another sub-table.
    (define (insert! keys value)

      ;; Inserts the remaining keys as sub-tables in table, until the last which will be stored with the value as a record.
      (define (insert-remaining-keys sub-keys prev-table)
	(cond ((null? sub-keys) 'done) ;; This wouldn't happen unless sub-keys was originally null passed in
	      ((null? (cdr sub-keys))
	       ;; car is the last key, so set to the new record instead of a table.
	       (set-cdr! prev-table (cons (make-record (car sub-keys) value) (cdr prev-table)))
	       'done)
	      (else
		;; Insert a new table, and proceed to the next key
		(let ((new-table (cons (car sub-keys) '())))
		  (set-cdr! prev-table (cons new-table (cdr prev-table)))
		  (insert-remaining-keys (cdr sub-keys) new-table)))))

      (define (iter sub-keys sub-table)
	(cond ((null? sub-keys)
	       ;; Ran out of keys, which means we've reached our target record. Sub table is the record since assoc returns the car of the pair which is the record instead of another table here.
	       (set-record-value! sub-table value))
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
	  (if (table? sub-table) sub-table (record-value sub-table))
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

(define (make-tree entry left right same-entry? greater-than-entry? lesser-than-entry?)
  (let ((tree (cons entry (cons left right))))
    (define (entry) (car tree))
    (define (left-branch) (cadr tree))
    (define (right-branch) (cddr tree))
    (define (set-left-branch! value)
      (set-car! (cdr tree) value))
    (define (set-right-branch! value)
      (set-cdr! (cdr tree) value))
 
    ;; NOTE: only works with binary trees (balanced or unbalanced, preferably balanced for best performance)
    (define (lookup target-entry)
      (cond ((same-entry? target-entry (entry)) #t)
	    ((and (greater-than-entry? target-entry (entry)) (not (null? (right-branch)))) (((right-branch) 'lookup) target-entry))
	    ((and (lesser-than-entry? target-entry (entry)) (not (null? (left-branch)))) (((left-branch) 'lookup) target-entry))
	    (else #f)))

    ;; (trace same-entry?)
    ;; (trace greater-than-entry?)
    ;; (trace lesser-than-entry?)


    ;; Prints in ascending order if the the tree is binary.
    (define (print tree)
      (if (or (null? tree))
	(println "this shouldn't happen")
	(let ((left (left-branch))
	      (right (right-branch))
	      (entry (entry)))
	  (if (null? left)
	    'done
	    (left 'print))
	  (println entry)
	  (if (null? right)
	    'done
	    (right 'print)))))
    (define (dispatch m)
      (cond ((eq? m 'entry) (entry))
	    ((eq? m 'left-branch) (left-branch))
	    ((eq? m 'rigth-branch) (right-branch))
	    ((eq? m 'set-left-branch!) set-left-branch!)
	    ((eq? m 'set-right-branch!) set-right-branch!)
	    ((eq? m 'lookup) lookup)
	    ((eq? m 'print) (print tree))
	    (else (error "unknown operation TREE"))))
    dispatch))

(main)
