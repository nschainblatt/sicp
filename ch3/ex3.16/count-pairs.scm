;; This count-pairs solution is invalid because it doesn't account for circular references, it should only count each distinct
;; pair once and detect cycles.

(define (main)
  (let* ((pair1 (cons 1 2))
	 (pair2 (cons pair1 pair1))
	 (linked-pair3 (cons 3 'todo))
	 (linked-pair2 (cons 2 linked-pair3))
	 (linked-pair1 (cons 1 linked-pair2)))
    (set-cdr! linked-pair3 linked-pair1)
    (println (count-pairs (cons 1 (cons 2 (cons 3 '()))))) ;; 3
    (println (count-pairs (cons (cons 1 pair1) pair1)))    ;; 4
    (println (count-pairs (cons pair2 pair2)))             ;; 7
    (println (count-pairs linked-pair3))))                 ;; doesn't finish

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define (println x)
  (display x) (newline))

(main)
