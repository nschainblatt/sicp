;; When deleting an item from the queue, only the front pointer is modified because the procedure that checks
;; if the queue is empty only checks the front pointer.
;; You can see that when I insert 'z after deleting 'b from the queue, that the extra 'b value is replaced by 'z.
;; Also, when the queue is empty, and you add an item, both the front and rear pointers become the same new-pair, they share
;; the reference. So later, when another item is added to the queue, the shared reference is modified, leaving both the front
;; and the rear pointer updated.

(define (main)
  (define q1 (make-queue))
  (print-queue (insert-queue! q1 'a))
  (print-queue (insert-queue! q1 'b))
  (print-queue (delete-queue! q1))
  (print-queue (delete-queue! q1))
  (println (empty-queue? q1))
  (print-queue (insert-queue! q1 'z))
  'done)

(define (print-queue queue)
  (println (front-ptr queue)))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	    (set-cdr! (rear-ptr queue) new-pair)
	    (set-rear-ptr! queue new-pair)
	    queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else (set-front-ptr! queue (cdr (front-ptr queue)))
	      queue)))

(define (println x)
  (display x) (newline))

(main)
