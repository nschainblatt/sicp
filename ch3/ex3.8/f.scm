(define (main)
  (define fl (make-f))
  (println (+ (fl 0) (fl 1))) ;; 0
  (define fr (make-f))
  (println (+ (fr 1) (fr 0)))) ;; 1

(define (make-f)
  (let ((state '()))
    (define (f x)
      (if (null? state)
        (begin (set! state x) 0)
        state))
    f))
