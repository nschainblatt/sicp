(define (main)
  (let* ((list1 (list 1 2 3))
	 (linked-pair3 (cons 3 'todo))
	 (linked-pair2 (cons 2 linked-pair3))
	 (linked-pair1 (cons 1 linked-pair2)))
    (set-cdr! linked-pair3 linked-pair1)
    (println (has-cycle? (cons 1 (cons 2 (cons 3 '()))))) ;; Not a cycle
    (println (has-cycle? linked-pair1))                   ;; Yes, it's a cycle
    (println (has-cycle? list1))                          ;; Not a cycle
    (println list1)
    'done))

;; Cycle detection with constant space using assignment.
(define (has-cycle? mylist)

  (define (tag x) (cons 'visited x))
  (define (type-tag x) (car x))
  (define (visited? x) (and (pair? x) (eq? (type-tag x) 'visited)))

  (define (iter x)
    (cond ((null? x) (remove-tags! mylist) #f)
	  ((visited? (car x)) (remove-tags! mylist) #t)
	  (else (set-car! x (tag (car x))) (iter (cdr x)))))

  (define (remove-tags! x)
    (define (iter sub-x)
      (cond ((null? sub-x) x)
	    ((visited? (car sub-x)) (set-car! sub-x (cdar sub-x)) (iter (cdr sub-x)))))
    (iter x))

  (iter mylist))


(define (println x)
  (display x) (newline))

(main)
