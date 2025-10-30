;; a. Note that this is not atomic and will produce unsafe results.
;;    This make-semaphore procedure follows the same structure as make-mutex, storing n as the cell value instead of a boolean.
;;    I have also localized the clear! and test-and-set! procedures to not interfere with the equivalent mutex global procedures.
(define (make-semaphore n)
  (let ((cell (list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set!)
	       (the-semaphore 'acquire))) ; retry
	    ((eq? m 'release) (clear!))))

    ;; Increments semaphore count by 1 when a process releases the semaphore.
    (define (clear!) (set-car! cell (+ (car cell) 1)))

    (define (test-and-set!)
      (if (= (car cell) 0) #t (begin (set-car! cell (- (car cell) 1) #f))))

    (if (< n 1)
      (error "Semaphore count must be at least 1.")
      the-semaphore)))


;; b. Note that both clear! and test-and-set! must be atomic because they alter the local state of the semaphore based
;;    on it's previous value.
;;    Note that this only works on single core machines, a different method using the hardware inside multi-core processors
;;    is required to make sure more than one thread on different cores doesn't access the same procedure.
(define (make-semaphore n)
  (let ((cell (list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set!)
	       (the-semaphore 'acquire))) ; retry
	    ((eq? m 'release) (clear!))))

    ;; Note that it is very important for the clear! and test-and-set! procedures to not be allowed to run at the same time,
    ;; otherwise the value of cell will be compromised.

    (define (clear!)
      (without-interrupts
	(lambda () (set-car! cell (+ (car cell) 1)))))

    (define (test-and-set!)
      (without-interrupts
	(lambda ()
	  (if (= (car cell) 0) #t (begin (set-car! cell (- (car cell) 1) #f))))))

    (if (< n 1)
      (error "Semaphore count must be at least 1.")
      the-semaphore)))
