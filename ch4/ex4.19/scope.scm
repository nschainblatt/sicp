;; I support Ben's approach, I prefer to allow the use of an outer-scoped variable until an inner definition overrides it.

;; Although footnote 26 mentions that mit-scheme would throw an error for this code snippet, however I encounter no such error.


(newline)
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (display (f 10)))



;; This is what would currently happen with the previous implmentation (Alyssa's proposed method that signals unassigned error)

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;; |
;; |
;; v

((lambda (a)
   ((lambda (x)
      (let ((b '*unassigned*)
	    (a '*unassigned*))
	(set! b (+ a x)) ;; <-- Unassigned error
	(set! a 5)
	(+ a b)))
    10))
 1)

;; In order for Eva's method to be implemented, we would have to search all definitions in a procedure for cyclic references and order them in the best way possible
;; to have no unassigned errors. Then, if it is still impossible because there are multiple conflicting references, we would signal an error.

;; So for this single instance we could set! 'a' before 'b' and there would be no issue, however, it would not be this easy in a more complex scenario. Which is why
;; Alyssa and footnote 26 suggest to signal an unassigned error.

;; Seeing that there is a version of variable 'a' in the outer scope, we could defer unassignment of 'a' until after the usages, however we would need to make sure certain
;; expressions use the new value if they were placed after the reassignment to 'a', hence the complexity of this problem.

;; A simple solution to these problems would be to use Alyssa's method with unassignment, however we check if a variable is unassigned, if it is, look in the outer scopes
;; for the variable and use that version instead. For expressions that come after the reassignment, they would just use the newest version (5 in this example) of the variable.
