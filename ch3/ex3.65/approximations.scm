(define (partial-sum s)
  (define x (cons-stream (stream-car s) (add-series x (stream-cdr s))))
  x)

(define (add-series s1 s2)
  (stream-map + s1 s2))

(define (summands n inc)
  (cons-stream (/ 1.0 n)
	       (stream-map - (summands (+ n inc) inc))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn 1
	(s1 (stream-ref s 1)) ; Sn
	(s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (print-n-stream stream n)
  (newline)
  (define (iter s i)
    (if (< i n)
      (begin (display (stream-car s)) (display " ") (iter (stream-cdr s) (+ i 1)))
      'done))
  (iter stream 0))

(define natural-log-of-two-stream
  (partial-sum (summands 1 1)))

(define approximations (make-tableau euler-transform natural-log-of-two-stream))

(newline)
(display "\nno acceleration")
(print-n-stream (stream-ref approximations 0) 1500) ;; Takes forever to converge, at 1500 it still hasn't converged.
(newline)
(display "\n1st accelerated sequence")
(print-n-stream (stream-ref approximations 1) 500) ;; Still slow, but a little faster.
(newline)
(display "\n2nd accelerated sequence")
(print-n-stream (stream-ref approximations 2) 500) ;; "
(newline)
(display "\n5th accelerated sequence")
(print-n-stream (stream-ref approximations 5) 10) ;; "
