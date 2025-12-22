(cd "../amb-eval")
(load "amb-eval.scm")

;; Task:
;; Update analyze-amb to choose a choice at random, instead of sequentially moving through options.
;; Make sure not to choose the same path twice.

;; We can choose an index of the list at random.
;; However we need a way to remove this option from further use.
;; We can do this by creating an iterator that builds up a new list as we are going to the random index.
;; Then it returns a new list without the current option.

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	  (fail)
	  (let* ((rpair (random-choice-pair choices))
		 (random-choice (car rpair))
		 (rest-choices (cdr rpair)))
	    (random-choice env
			   succeed
			   (lambda ()
			     (try-next rest-choices))))))
      (try-next cprocs))))

;; Returns a pair of the random choice, and the remaining choices without the chosen one.
(define (random-choice-pair choices)
  (let ((target (random-index choices)))
    (define (iter index pre rest)
      (if (= index target)
	(cons (car rest) (append pre (cdr rest)))
	(iter (inc index) (append pre (list (car rest))) (cdr rest))))
    (iter 0 '() choices)))

;; Returns a random index in range of seq.
(define (random-index seq)
  (random (length seq)))

(define (inc x)
  (+ x 1))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

;; ---

(define the-global-environment (setup-environment))
(driver-loop)
