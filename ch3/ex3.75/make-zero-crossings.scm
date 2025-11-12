(define input-list '(1 2 -1 -2 0 -1 0 2 4 5))
(define tmp (list->stream input-list))
(define sense-data (stream-append tmp (cons-stream 0 tmp))) ;; Create a cycle to not encounter finite stream limitations (null access)

(define (pos? x)
  (>= x 0))

(define (neg? x)
  (< x 0))

(define (sign-change-detector current-value last-value)
  (cond ((and (neg? last-value) (pos? current-value)) 1)
	((and (pos? last-value) (neg? current-value)) -1)
	(else 0)))

;; Alyssa's system
(define (original-make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (original-make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define original-zero-crossings
  (original-make-zero-crossings sense-data 0))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
		    last-value)
		 2)))

    (cons-stream
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings
	(stream-cdr input-stream) (stream-car input-stream) avpt))))

(define zero-crossings
  (make-zero-crossings sense-data 0 0))

(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(print-n-stream original-zero-crossings (length input-list))
(print-n-stream zero-crossings 10)
