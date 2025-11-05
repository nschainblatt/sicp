(define (println x)
  (newline) (display x))

(println "\nPart a:")

(define (integrate-series stream-a)
  (stream-map-with-index (lambda (index value) (* (/ 1 index) value)) stream-a))

(define (stream-map-with-index proc s)
  (define (recur sub index)
    (if (null? sub)
      '()
      (cons-stream (proc index (stream-car sub)) (recur (stream-cdr sub) (+ index 1)))))
  (recur s 1))

(define (stream-for-each-until proc s ref)
  (cond ((null? s) 'done)
	((< ref 1) 'done)
	(else (proc (stream-car s)) (stream-for-each-until proc (stream-cdr s) (- ref 1)))))

(define (print x)
  (display x) (display " "))


(define (stream-enumerate-interval low high)
  (if (> low high) '()
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define s1 (stream-enumerate-interval 3 20))

(newline)
(stream-for-each-until print s1 10)
(newline)
(stream-for-each-until print (integrate-series s1) 10)


(println "\nPart b:")

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(newline)
(stream-for-each-until print exp-series 10)

(newline)

(println "cosine")
(newline)
(stream-for-each-until print cosine-series 20)
(println "sine")
(newline)
(stream-for-each-until print sine-series 20)
