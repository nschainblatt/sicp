;; The main difference between the streams from chapter 3 and the ones we created in our language is that both the car and the cdr
;; of the stream in our language are delayed. While in chapter 3, just the cdr was delayed.

;; Assuming the expressions within the car and the cdr don't use assignment, having them both be delayed can bring some advantages
;; over regular streams from chapter 3.

;; One advantage is the ability to create fully lazy-evaluated data structures, such as lazy trees.
;; This is possible since both the car and cdr are delayed, thus both pointers can hold sub-trees of their own that remain unevaluated
;; until they are needed.
;; So as you traverse the tree, you only evaluate the branches you've encountered, not every branch in the tree.

;; Potential disadvantages of having the stream be fully delayed would be if you introduced assignment. This is can lead to unwanted
;; side effects if you depend on the value of a variable that changes over time, with the expression that performs
;; the assignment being delayed.


;; Example tree (must be evaluated in our fully lazy evaluator lazy-eval.scm)
;; Balanced lazy binary tree

;; Each value, left and right branches are delayed, only being fully evaluated when they are needed (called with primitive or printed
;; to screen)

(define (make-tree value left right)
  (cons value (cons left right)))
(define the-empty-tree '())

(make-tree 5
	   (make-tree 3
		      (make-tree 1
				 the-empty-tree
				 the-empty-tree)
		      (make-tree 2
				 the-empty-tree
				 the-empty-tree))
	   (make-tree 7
		      (make-tree 6
				 the-empty-tree
				 the-empty-tree)
		      (make-tree 8
				 the-empty-tree
				 the-empty-tree)))

