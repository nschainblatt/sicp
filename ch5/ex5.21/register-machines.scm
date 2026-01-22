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
	(assign n (const 0))
	(assign continue (label count-done))
	
      count-leaves-car
	(test (op null?) (reg tree))
	(restore continue)
	(restore tree)
	(branch (reg continue))

	(test (op pair?) (reg tree))
	(save tree)
	(assign tree (op car) (reg tree))
	(save continue)
	(assign continue (label count-leaves-cdr))
	(branch count-leaves-car)

	(assign n (op +) (reg n) (const 1))
	(restore continue)
	(restore tree)
	(goto (reg continue))

      count-leaves-cdr
	(assign tree (op cdr) (reg tree))
	(goto (label count-leaves-car))

      count-done
      ;; Get register contents of n to see results.
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

(define count-leaves-machine-2
  (make-machine
    (list (list '+ +))
    '(
	(assign n (const 0))
	(assign continue (label count-done))
	(save tree)
	(save continue)
	(assign continue (label count-leaves))
	
      count-leaves
	(test (op null?) (reg tree))
	(restore continue)
	(restore tree)
	(branch (reg continue))

	(test (op pair?) (reg tree))
	(assign temp (op car) (reg tree))
	(assign tree (op cdr) (reg tree))
	(save tree)
	(save continue)
	(assign continue (label count-leaves))
	(assign tree (reg temp))
	(branch count-leaves)

	(assign n (op +) (reg n) (const 1))
	(restore continue)
	(restore tree)
	(goto (reg continue))

      count-done
      ;; Get register contents of n to see results.
      )))
