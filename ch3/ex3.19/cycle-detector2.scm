(define (main)
  (let* ((list1 (list 1 2 3))
	 (linked-pair3 (cons 3 'todo))
	 (linked-pair2 (cons 2 linked-pair3))
	 (linked-pair1 (cons 1 linked-pair2)))
    (set-cdr! linked-pair3 linked-pair1)
    (println (has-cycle? (cons 1 (cons 2 (cons 3 '()))))) ;; Not a cycle
    (println (has-cycle? linked-pair1))                   ;; Yes, it's a cycle
    (println (has-cycle? list1))                          ;; Not a cycle
    'done))

;; Cycle detection with constant space using tortoise and hare algorithm.
(define (has-cycle? mylist)

  (define (iter left right)
    (cond ((null? right) #f)
	  ((eq? (car left) (car right)) #t)
	  (else (iter (cdr left) (cddr right)))))

  (iter mylist (cdr mylist)))

(define (println x)
  (display x) (newline))

(main)
