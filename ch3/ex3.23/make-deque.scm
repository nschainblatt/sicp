(define (main)
  (newline)
  (define q1 (make-queue))
  (println (q1 'empty?))  ; #t
  (q1 'print)             ; '()
  ((q1 'insert-front) 'a)
  (q1 'print)             ; a
  ((q1 'insert-front) 'b)
  (q1 'print)             ; ba
  ((q1 'insert-front) 'b)
  (q1 'print)             ; bba
  ((q1 'insert-front) 'c)
  (q1 'print)             ; cbba
  ((q1 'insert-rear) 'z)
  (q1 'print)             ; cbbaz
  (println (q1 'front))   ; c
  (println (q1 'rear))    ; z
  (q1 'delete-front)
  (q1 'print)             ; bbaz
  (q1 'delete-front)
  (q1 'print)             ; baz
  (q1 'delete-front)
  (q1 'print)             ; az
  (q1 'delete-rear)
  (q1 'print)             ; a
  (q1 'delete-rear)
  (q1 'print)             ; '()
  (println (q1 'empty?))  ; #t
  'done)

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (make-node x prev next)
      (cons (cons x prev) next))
    (define (node-value node)
      (caar node))
    (define (node-prev node)
      (cdar node))
    (define (node-next node)
      (cdr node))
    (define (set-node-next! node new-next)
      (set-cdr! node new-next))
    (define (set-node-prev! node new-prev)
      (set-cdr! (car node) new-prev))

    (define (empty?) (null? front-ptr))

    (define (insert-front! x)
      (if (empty?)
	(create-default-deque x)
	(let* ((new-node (make-node x rear-ptr front-ptr)))
	  ;; 1. Update existing front prev
	  ;; 2. Update rear next
	  ;; 3. Update to new front
	  (set-node-prev! front-ptr new-node)
	  (set-node-next! rear-ptr new-node)
	  (set-front! new-node))))

    (define (insert-rear! x)
      (if (empty?)
	(create-default-deque x)
	(let* ((new-node (make-node x rear-ptr front-ptr)))
	  ;; 1. Update existing rear next
	  ;; 2. Update existing front prev
	  ;; 3. Update to new rear
	  (set-node-next! rear-ptr new-node)
	  (set-node-prev! front-ptr new-node)
	  (set-rear! new-node))))

    (define (create-default-deque x)
      (let* ((new-node (make-node x '() '())))
	(set-node-prev! new-node new-node)
	(set-node-next! new-node new-node)
	(set-front! new-node)
	(set-rear! new-node)))

    (define (delete-front!)
      (cond ((empty?) (error "Queue is empty, cannot delete any items"))
	    ((eq? (node-next front-ptr) rear-ptr) (set-front! rear-ptr))
	    ((eq? front-ptr rear-ptr) (set-front! '()) (set-rear! '()))
	    (else
	      ;; 1. Update existing rear to have a next of the next of the existing front.
	      ;; 2. Update the new front to have a prev to the existing rear.
	      ;; 3. Update front to the next of the existing front.
	      (set-node-next! rear-ptr (node-next front-ptr))
	      (set-node-prev! (node-next front-ptr) rear-ptr)
	      (set-front! (node-next front-ptr)))))

    (define (delete-rear!)
      (cond ((empty?) (error "Queue is empty, cannot delete any items"))
	    ((eq? (node-next front-ptr) rear-ptr) (set-rear! front-ptr))
	    ((eq? front-ptr rear-ptr) (set-front! '()) (set-rear! '()))
	    (else
	      ;; 1. Update existing rear prevs next ptr to be front
	      ;; 2. Update front prevs ptr to be existing rears prev
	      ;; 3. Update rear to be the prev of the existing rear.
	      (set-node-next! (node-prev rear-ptr) front-ptr)
	      (set-node-prev! front-ptr (node-prev rear-ptr))
	      (set-rear! (node-prev rear-ptr)))))

    ;; Print from front-ptr to rear-ptr then stop to avoid cycling.
    ;; NOTE: when a single item is added to an empty deque, it only prints the item once even though it exists as both
    ;; the front and end nodes because they are the same reference, leading to eq? => #t.
    (define (print node)
      (define (iter curr)
	(cond ((null? curr) (display '()) (newline))
	      ((eq? curr rear-ptr) (display (node-value curr)) (newline))
	      (else (display (node-value curr)) (iter (cdr curr)))))
      (iter node))

    (define (set-front! new-value) (set! front-ptr new-value))
    (define (set-rear! new-value) (set! rear-ptr new-value))

    (define (front)
      (if (empty?)
	(error "Queue is empty, cannot retrieve any items")
	(node-value front-ptr)))

    (define (rear)
      (if (empty?)
	(error "Queue is empty, cannot retrieve any items")
	(node-value rear-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
	    ((eq? m 'insert-front) insert-front!)
	    ((eq? m 'insert-rear) insert-rear!)
	    ((eq? m 'delete-front) (delete-front!))
	    ((eq? m 'delete-rear) (delete-rear!))
	    ((eq? m 'front) (front))
	    ((eq? m 'rear) (rear))
	    ((eq? m 'print) (print front-ptr))
	    (else (error "Unknown operation --deque"))))
    dispatch))

(define (println x)
  (display x) (newline))

(main)
