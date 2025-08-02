(define (main)
  (define x (list (list 1 2) (list 3 4)))
  (display (fringe x))
  (newline)
  (display (fringe (list x x)))
  (newline)
  (display (fringe (list (list 1 (list 2 x)) (list 4 (list 5 x)))))
  (newline))


;; Returns a list of all the leaves of the 'tree' in left-to-right order.
(define (fringe tree)

  ;; Traverse the trees nodes, each node has a left and right value: a list (potentially empty) or a value (leaf).
  ;; Each nodes left and right values are traversed until a leaf or nil is found, both are added to the ongoing list, maintaining
  ;; a proper list. The append procedure is utilized to build the proper list, the items to append to the list are
  ;; either values (leafs) or empty lists, we have to convert any leaves to lists in order for append to work correctly.
  (define (get-all-leaves node)
    (cond ((null? node) '())
          ((list? node)
           (append (get-as-list (get-all-leaves (car node))) (get-as-list (get-all-leaves (cdr node)))))
          (else node)))

  (get-all-leaves tree))


(define (get-as-list x)
  (if (list? x) x
    (list x)))
