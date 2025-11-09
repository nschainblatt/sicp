(define input-list (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define sense-data (list->stream input-list))

(define (sign-change-detector current-value last-value)
  (cond ((and (negative? last-value) (positive? current-value)) 1)
	((and (positive? last-value) (negative? current-value)) -1)
	(else 0)))

;; Alyssa's system
(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))
(define alyssas-zero-crossings
  (make-zero-crossings sense-data 0))

;; Alyssa's bosses system
(define zero-crossings
  (stream-map sign-change-detector
	      sense-data
	      (cons-stream 0 sense-data)))


(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(print-n-stream alyssas-zero-crossings (- (length input-list) 1))
(print-n-stream zero-crossings (- (length input-list) 1))
