(define random-init 5)
(define random-max 1000000000)

;; This will produce the same sequence of 4 random numbers.
(define (main)
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))

  (display "Resetting seed to: ")
  (println (rand 'reset random-init))

  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate)))

(define rand
  (let ((x random-init))
    (define (random)
      (set! x (rand-update x))
      x)

    (define (reset new-value)
      (set! x new-value) new-value)

    (define (dispatch m . args)
      (cond ((eq? m 'generate) (random))
	    ((eq? m 'reset) (reset (car args)))
	    (else (error "INVALID MESSAGE --RAND"))))
    dispatch))

;; Implementation of rand-update found online
(define (rand-update x0)
  (let* ((x1 (bitwise-xor x0 (arithmetic-shift x0 -13)))
	 (x2 (bitwise-xor x1 (arithmetic-shift x1 18))))
    (bitwise-and x2 random-max)))
