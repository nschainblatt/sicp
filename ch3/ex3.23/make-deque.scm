(define (main)
  (newline)
  (define q1 (make-deque))
  (println (empty-deque? q1))  ; #t
  (print-deque q1)             ; '()
  (front-insert-deque! q1 'a)
  (print-deque q1)             ; a
  (front-insert-deque! q1 'b)
  (print-deque q1)             ; ba
  (front-insert-deque! q1 'b)
  (print-deque q1)             ; bba
  (front-insert-deque! q1 'c)
  (print-deque q1)             ; cbba
  (rear-insert-deque! q1 'z)
  (print-deque q1)             ; cbbaz
  (println (front-deque q1))   ; c
  (println (rear-deque q1))    ; z
  (front-delete-deque! q1)
  (print-deque q1)             ; bbaz
  (front-delete-deque! q1)
  (print-deque q1)             ; baz
  (front-delete-deque! q1)
  (print-deque q1)             ; az
  (rear-delete-deque! q1)
  (print-deque q1)             ; a
  (rear-delete-deque! q1)
  (print-deque q1)             ; '()
  (println (empty-deque? q1))  ; #t
  'done)

(define (empty-deque? q)
  (q 'empty?))

(define (front-deque q)
  (q 'front))

(define (rear-deque q)
  (q 'rear))

(define (front-insert-deque! q x)
  ((q 'insert-front) x))

(define (rear-insert-deque! q x)
  ((q 'insert-rear) x))

(define (front-delete-deque! q)
  (q 'delete-front))

(define (rear-delete-deque! q)
  (q 'delete-rear))

(define (print-deque q)
  (q 'print))

(define (make-deque)
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
      (cond ((empty?) (error "Deque is empty, cannot delete any items"))
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
      (cond ((empty?) (error "Deque is empty, cannot delete any items"))
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
	(error "Deque is empty, cannot retrieve any items")
	(node-value front-ptr)))

    (define (rear)
      (if (empty?)
	(error "Deque is empty, cannot retrieve any items")
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
