;; Exercise 5.21: Implement register machines for the follow-
;; ing procedures. Assume that the list-structure memory op-
;; erations are available as machine primitives.

;; Note that I am also assuming that null?, pair?, car, and cdr are all available as primitives
;; since I wouldn't want to place them in the-ops because that would require assigning the lisp
;; behavior of those built-ins.

;; a. Recursive count-leaves:

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))


(define count-leaves-recursive-machine
  (make-machine
    (list (list '+ +))
    '(

      count-leaves
	(test (op null?) (reg tree))








      )))


;; b. Recursive count-leaves with explicit counter:

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
	  ((not (pair? tree)) (+ n 1))
	  (else
	    (count-iter (cdr tree)
			(count-iter (car tree)
				    n)))))
  (count-iter tree 0))

;; ((1 2) (3 4)) => 4

(define count-leaves-machine-2
  (make-machine
    (list (list '+ +))
    '(
      (assign (reg n) (const 0))
      (assign (reg continue) (label count-done))
      (save continue)
      (assign coninue (label count-leaves))
      count-leaves
	(test (op null?) (reg tree))
	(restore tree) ;; FIXME: at the end, due to our uneven saves, we don't have a tree to restore.
	(restore continue)
	(branch (reg continue))

	(test (op pair?) (reg tree))
	(assign temp (op car) (reg tree))
	(assign tree (op cdr) (reg tree))
	(save continue)
	(save tree)
	(assign continue (label count-leaves))
	(assign tree (reg temp))
	(branch count-leaves)

	(assign n (op +) (reg n) (const 1))
	(restore tree)
	(restore continue)
	(goto (reg continue))

      count-done
      ;; Get register contents of n to see results.
      )))
