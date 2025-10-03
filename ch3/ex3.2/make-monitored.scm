(define (main)
  (define monitored-square (make-monitored square))
  (println (monitored-square 5))                 ;; 25
  (println (monitored-square 8))                 ;; 64
  (println (monitored-square 'how-many-calls?))  ;; 2
  (monitored-square 'reset-count)
  (println (monitored-square 'how-many-calls?))) ;; 0

(define (make-monitored f)
  (let ((count 0))
    (define (mf arg)
      (set! count (+ count 1))
      (f arg))
    (define (reset)
      (set! count 0))
    (define (dispatch message)
      (cond ((eq? message 'how-many-calls?) count)
	    ((eq? message 'reset-count) (reset))
	    (else (mf message))))
    dispatch))

(define (square x) (* x x))
