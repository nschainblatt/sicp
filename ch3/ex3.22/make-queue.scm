(define (main)
  (define q1 (make-queue))
  (println (q1 'empty?))      ;; #t
  (println ((q1 'insert) 'a)) ;; (a)
  (println ((q1 'insert) 'b)) ;; (a b)
  (println (q1 'front))       ;; a
  (println (q1 'rear))        ;; b
  (println (q1 'delete))      ;; (b)
  (println (q1 'empty?))      ;; #f
  (q1 'print)                 ;; (b)
  (println (q1 'delete))      ;; ()
  (println (q1 'empty?))      ;; #t
  (println (q1 'delete))      ;; error
  'done)

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (empty?) (null? front-ptr))
    (define (insert! x)
      (let ((new-item (cons x '())))
	(if (empty?)
	  (begin (set-front! new-item) (set-rear! new-item) front-ptr)
	  (begin (set-cdr! rear-ptr new-item) (set-rear! new-item) front-ptr))))
    (define (delete!)
      (if (empty?)
	(error "Queue is empty, cannot delete any items")
	(begin (set-front! (cdr front-ptr)) front-ptr)))
    (define (print) (println front-ptr))
    (define (set-front! new-value) (set! front-ptr new-value))
    (define (set-rear! new-value) (set! rear-ptr new-value))
    (define (front)
      (if (empty?)
	(error "Queue is empty, cannot retrieve any items")
	(car front-ptr)))
    (define (rear)
      (if (empty?)
	(error "Queue is empty, cannot retrieve any items")
	(car rear-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
	    ((eq? m 'insert) insert!)
	    ((eq? m 'delete) (delete!))
	    ((eq? m 'front) (front))
	    ((eq? m 'rear) (rear))
	    ((eq? m 'print) (print))))
    dispatch))

(define (println x)
  (display x) (newline))

(main)
