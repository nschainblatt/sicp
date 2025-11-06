(define (stream-enumerate-interval low high)
  (if (> low high) '()
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-for-each-until proc s ref)
  (cond ((null? s) 'done)
	((< ref 1) 'done)
	(else (proc (stream-car s)) (stream-for-each-until proc (stream-cdr s) (- ref 1)))))

(define (println x)
  (newline) (display x))

(define (print x)
  (display x) (display " "))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

;; This uses the Cauchy product to build a result stream that is simplified (like terms joined).
;; This works with infinite streams because the Cauchy product can build up like terms along the way, without
;; requiring to know all terms in the infinite series.
;; The stream-car of the resulting stream is made on the terms in ascending order of power, this term
;; is the only one in the entire stream with it's power.
;; add-stream is used in the stream-cdr to add all like terms of the next power.
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

(define (stream-map-with-index proc s)
  (define (recur sub index)
    (if (null? sub)
      '()
      (cons-stream (proc index (stream-car sub)) (recur (stream-cdr sub) (+ index 1)))))
  (recur s 1))

(define (integrate-series stream-a)
  (stream-map-with-index (lambda (index value) (* (/ 1 index) value)) stream-a))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(newline)
(stream-for-each-until print (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)) 20)
