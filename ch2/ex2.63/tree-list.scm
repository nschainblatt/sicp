;; a. The tree to list procedures produce the same results lists for the same trees.
;; tree->list-1 is a recursive process that builds the result list from left branch to right branch keeping order.
;; tree->list-2 is a recursive process that builds the result list from left branch to right branch keeping order.
;;
;; b. The order of growth is larger for tree->list-2 than for tree->list-1. This is because in each step of tree->list-2 append is used to build
;; the results list and append has linear order of growth. Append is used at each node of the tree, and since at each node of the tree the 
;; remaining nodes are halved (on average), the order of growth with this append is O(logn), however, since every node is traversed while
;; building this list, the order of growth is O(n*logn).
;; While tree->list-1 uses cons instead of append, which is a constant time operation, the order of grown is O(n), which is based on just
;; the regular traversal visiting each node of the tree.

(define (main)
  (let ((tree1 (make-tree 7
                          (make-tree 3
                                     (make-tree 1 '() '())
                                     (make-tree 5 '() '()))
                          (make-tree 9
                                     '()
                                     (make-tree 11 '() '()))))
        (tree2 (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 7
                                     (make-tree 5 '() '())
                                     (make-tree 9
                                                '()
                                                (make-tree 11 '() '())))))
        (tree3 (make-tree 5
                          (make-tree 3
                                     (make-tree 1 '() '())
                                     '())
                          (make-tree 9
                                     (make-tree 7 '() '())
                                     (make-tree 11 '() '())))))
    (println (tree->list-1 tree1)) ;; 1 3 5 7 9 11
    (println (tree->list-2 tree1)) ;; "
    (println (tree->list-1 tree2)) ;; "
    (println (tree->list-2 tree2)) ;; "
    (println (tree->list-1 tree3)) ;; "
    (println (tree->list-2 tree3)))) ;; "

(define (println x)
  (display x)
  (newline))

(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)

;; Recursive process
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1
                    (right-branch tree))))))

;; Semi-Iterative process
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))
