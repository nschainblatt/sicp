(define (main)
  (let* ((pair1 (cons 1 2))
	 (pair2 (cons pair1 pair1))
	 (linked-pair3 (cons 3 'todo))
	 (linked-pair2 (cons 2 linked-pair3))
	 (linked-pair1 (cons 1 linked-pair2)))
    (set-cdr! linked-pair3 linked-pair1)
    (println (has-cycle? (cons 1 (cons 2 (cons 3 '()))))) ;; Not a cycle
    (println (has-cycle? linked-pair1))                   ;; Yes, it's a cycle
    'done))

(define (has-cycle? mylist)
  (let ((distinct-elements '()))

    (define (unique-element? element)
      (define (iter d-elements)
	(cond ((null? d-elements) #t)
	      ((eq? (car d-elements) element) #f)
	      (else (iter (cdr d-elements)))))
      (iter distinct-elements))

    (define (iter x)
      (cond ((null? x) #f)
	    ((not (unique-element? (car x))) #t)
	    (else (set! distinct-elements (cons (car x) distinct-elements)) (iter (cdr x)))))

    (iter mylist)))


(define (println x)
  (display x) (newline))

(main)
