;; a. The procedure 'list->tree' takes an ordered list of elements and builds a balanced tree.
;;    This procedure creates a recursive process to solve this problem, first by breaking down the problem
;;    on the left side first, and them working it's way back up and to the right. So it starts off by reaching the bottom left
;;    sub-tree, where n = 2 to make the left-tree be an empty tree and the right-tree be 3 (with empty leaves), leaving the first entry to be
;;    1. It then uses this sub-tree of (1 () (3 () ())) to return to be the left tree of the next call in the call stack.
;;    The next entry is 5, and the right tree is then built, by yet again reaching the recursive exit condition where n = 0.
;;    Now 7 is the left tree, 9 is the entry, and 11 is the right tree. Finally returning back up to the initial call, with 
;;    (1 () (3 () ())) being the left-tree, (9 (7 () ()) (11 () ())) being the right tree, and 5 being the final entry.
;;    leading to the tree below show below. Here it is in display format: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))).
;;
;; Tree produced for list (1 3 5 7 9 11)
;;       5
;;      / \
;;     /   9
;;    1     \
;;     \    /\
;;      3  7  11
;;
;; b. The order of growth is O(n). It may appear as logarithmic because we halve the input at each step, however we still end up traversing
;; all the elements, we just split the work between two seperate calls (left and right branches) leading O(n).


(define (main)
  (println (list->tree (list 1 3 5 7 9 11))))

(define (println x)
  (display x)
  (newline))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
