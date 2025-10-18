;; TODO:
;; 1. Update the table and sub-table representation to be a binary tree instead of just a list of records.
;;    - We will need a representation for a tree as well as selectors and a constructor.
;;    - We will need a procedure to create a tree from a list, to then use a procedure to turn a list into a balanced tree.
;;        - This is so we can keep a balanced tree for maximum performance, we can rebalance the tree using an internal counter,
;;          that tracks the number of inserts! made.
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
  ((table 'insert) (list 1 2) 'x2)
  ((table 'insert) (list 1 3) 'x3)

  ((table 'insert) (list 2 1) 'y1)
  ((table 'insert) (list 2 2) 'y2)
  ((table 'insert) (list 2 3) 'y3)

  ((table 'insert) (list 3 1) 'z1)
  ((table 'insert) (list 3 2) 'z2)
  ((table 'insert) (list 3 3) 'z3)

  ((table 'insert) (list 1 1) 'x1.1)
  ((table 'insert) (list 2 2) 'y2.2)
  ((table 'insert) (list 3 3) 'z3.3)

  ((table 'insert) (list 10 9 8 7 6 5 4 3 2 1) 'TEST)
  ((table 'insert) (list 10 9 8 7 6 5 4 3 2 1) 'TESTAGAIN)

  (table 'print)

  (newline)

  (println ((table 'lookup) (list 10 9 8 7 6 5 4 3 2 1))) ;; 'TESTAGAIN
  (println ((table 'lookup) (list 10 9 8 7 6 5 4 3 2)))   ;; 'TREE

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

  (define (same-entry? key entry)
    (equal? key (car entry)))

  (define (greater-than-entry? value entry)
    (> value (car entry)))

  (define (lesser-than-entry? value entry)
    (< value (car entry)))

  (let ((table (list '*table*)))

    ;; Note that an unexpected error could happen if keys were given in the wrong order, such as if you gave 3 keys, and the middle
    ;; key was to a record and not another sub-table.
    (define (insert! keys value)

      ;; Inserts the remaining keys as sub-tables in table, until the last which will be stored with the value as a record.
      (define (insert-remaining-keys sub-keys prev-table)
	(cond ((null? sub-keys) 'done) ;; This wouldn't happen unless sub-keys was originally null passed in
	      ((null? (cdr sub-keys))
	       ;; If no tree create a new tree, otherwise insert record into existing tree.
	       (if (null? (cdr prev-table))
		 (set-cdr! prev-table (make-tree (make-record (car sub-keys) value) '() '() same-entry? greater-than-entry? lesser-than-entry?))
		 (((cdr prev-table) 'insert!) (make-record (car sub-keys) value)))
	       'done)
	      (else
		;; Insert a new table, and proceed to the next key
		(let* ((new-pair (cons (car sub-keys) '()))
		      (new-tree (make-tree new-pair '() '() same-entry? greater-than-entry? lesser-than-entry?)))
		  (if (null? (cdr prev-table))
		    (begin (set-cdr! prev-table new-tree) (insert-remaining-keys (cdr sub-keys) (new-tree 'entry)))
		    (begin (((cdr prev-table) 'insert!) new-pair) (insert-remaining-keys (cdr sub-keys) new-pair)))))))

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
    ;; NOTE: requires each sub-tree to have a procedure to perform an appropriate same-key? comparison by extracting the
    ;; key from the record within the sub-trees entry and comparing it to the target key we pass in here.
    (define (myassoc key sub-tree)
      (cond ((null? sub-tree) #f)
	    (else ((sub-tree 'lookup) key))))

    (define (print)
      (define (iter sub-table)
	(if (not (null? (cdr sub-table)))
	  ((cdr sub-table) 'print)
	  'done))
      (iter table))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
	    ((eq? m 'lookup) lookup)
	    ((eq? m 'print) (print))
	    (else (error "Invalid operation MAKE-TABLE"))))
    dispatch))


(define (println x)
  (display x) (newline))

(define (make-tree entry left right same-entry? greater-than-entry? lesser-than-entry?)
  (let ((tree (cons entry (cons left right))))
    (define (entry) (car tree))
    (define (left-branch) (cadr tree))
    (define (right-branch) (cddr tree))
    (define (set-entry! value)
      (set-car! tree value))
    (define (set-left-branch! value)
      (set-car! (cdr tree) value))
    (define (set-right-branch! value)
      (set-cdr! (cdr tree) value))

    (define (insert! target-entry)
      (cond ((same-entry? (car target-entry) (entry)) (set-entry! target-entry))
	    ((greater-than-entry? (car target-entry) (entry))
	     (if (null? (right-branch)) (set-right-branch! (make-tree target-entry '() '() same-entry? greater-than-entry? lesser-than-entry?)) (((right-branch) 'insert!) target-entry)))
	    ((lesser-than-entry? (car target-entry) (entry))
	     (if (null? (left-branch)) (set-left-branch! (make-tree target-entry '() '() same-entry? greater-than-entry? lesser-than-entry?)) (((left-branch) 'insert!) target-entry)))
	    (else ("Illegal state exception"))))

    ;; NOTE: only works with binary trees (balanced or unbalanced, preferably balanced for best performance)
    (define (lookup target-entry)
      (cond ((same-entry? target-entry (entry)) (entry))
	    ((and (greater-than-entry? target-entry (entry)) (not (null? (right-branch)))) (((right-branch) 'lookup) target-entry))
	    ((and (lesser-than-entry? target-entry (entry)) (not (null? (left-branch)))) (((left-branch) 'lookup) target-entry))
	    (else #f)))

    ;; Prints in ascending order if the the tree is binary.
    (define (print)
      (if (or (null? tree))
	(println "this shouldn't happen")
	(let ((left (left-branch))
	      (right (right-branch))
	      (entry (entry)))
	  (if (null? left)
	    'done
	    (left 'print))
	  (if (tree? (cdr entry))
	    (begin (println (car entry)) ((cdr entry) 'print))
	    (println entry))
	  (if (null? right)
	    'done
	    (right 'print))))
      dispatch)
    (define (dispatch m)
      (cond ((eq? m 'entry) (entry))
	    ((eq? m 'left-branch) (left-branch))
	    ((eq? m 'right-branch) (right-branch))
	    ((eq? m 'set-left-branch!) set-left-branch!)
	    ((eq? m 'set-right-branch!) set-right-branch!)
	    ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'print) (print))
	    (else (error "unknown operation TREE"))))
    dispatch))

(define tree? procedure?)

(main)
