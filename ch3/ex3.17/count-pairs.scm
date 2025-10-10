(define (main)
  (let* ((pair1 (cons 1 2))
	 (pair2 (cons pair1 pair1))
	 (linked-pair3 (cons 3 'todo))
	 (linked-pair2 (cons 2 linked-pair3))
	 (linked-pair1 (cons 1 linked-pair2)))
    (set-cdr! linked-pair3 linked-pair1)
    (println (count-distinct-pairs (cons 1 (cons 2 (cons 3 '()))))) ;; 3
    (println (count-distinct-pairs (cons (cons 1 pair1) pair1)))    ;; 3
    (println (count-distinct-pairs (cons pair2 pair2)))             ;; 3
    (println (count-distinct-pairs linked-pair3))                   ;; 3
    (println (count-distinct-pairs (list 1 2 3 (list 1 2 (list 3) (list 2))))) ;; 10
    'done))

(define (count-distinct-pairs pairs)
  (let ((distinct-pairs '()))

    (define (unique-pair? pair)
      (define (iter d-pairs)
	(cond ((null? d-pairs) #t)
	      ((eq? (car d-pairs) pair) #f)
	      (else (iter (cdr d-pairs)))))
      (iter distinct-pairs))

    (define (counter x)
      (if (or (not (pair? x)) (not (unique-pair? x)))
	0
	(begin (set! distinct-pairs (cons x distinct-pairs)) (+ (counter (car x))
					     (counter (cdr x))
					     1))))

    (counter pairs)))

(define (println x)
  (display x) (newline))

(main)
