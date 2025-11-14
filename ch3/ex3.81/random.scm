(define (random-numbers input-stream random-init)

  (define (random-number-stream init)
    (cons-stream
      init
      (stream-map rand-update (random-number-stream init))))

  (define (random-request-handler inner-input-stream inner-random-number-stream)
    (if (null? inner-input-stream)
      '()
      (let ((request (car inner-input-stream)))
	(cond ((eq? (car request) 'generate)
	       (cons-stream (stream-car inner-random-number-stream) (random-request-handler (stream-cdr inner-input-stream) (stream-cdr inner-random-number-stream))))
	      ((eq? (car request) 'reset)
	       (random-request-handler (stream-cdr inner-input-stream) (stream-cdr (random-number-stream (cdr request)))))
	      (else (error "INVALID REQUEST --RANDOM-NUMBERS"))))))

  (random-request-handler input-stream (stream-cdr (random-number-stream random-init))))

;; Implementation of rand-update found online
(define (rand-update x0)
  (define random-max #x7fffffff) ;; max signed 32 bit int
  (let* ((x1 (bitwise-xor x0 (arithmetic-shift x0 -13)))
	 (x2 (bitwise-xor x1 (arithmetic-shift x1 18))))
    (bitwise-and x2 random-max)))

(define (println x)
  (newline)
  (display x))

(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(define requests (cons-stream
		   (cons 'generate '())
		   (cons-stream 
		     (cons 'generate '())
		     (cons-stream
		       (cons 'generate '())
		       (cons-stream (cons 'reset 12345) requests)))))

(print-n-stream (random-numbers requests 12345) 20)
