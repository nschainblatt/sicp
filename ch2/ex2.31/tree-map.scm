(define (main)
  (square-tree (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7))))

(define (square-tree tree)
  (tree-map square tree))

;; Applies the given procedure to every leaf of the tree
(define (tree-map fn tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (tree-map fn (car tree)) (tree-map fn (cdr tree))))
        (else (fn tree))))

(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (cond ((null? sub-tree) '())
               ((pair? sub-tree) (tree-map fn sub-tree))
               (else (fn sub-tree))))
       tree))

(define (square x) (* x x))
