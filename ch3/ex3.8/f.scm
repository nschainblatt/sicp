(define (main)
  (define fl (make-f))
  (println (+ (fl 0) (fl 1))) ;; 0
  (define fr (make-f))
  (println (+ (fr 1) (fr 0)))) ;; 1

(define (make-f)
  (let ((state 0))
    ;; f always returns the previous state.
    (define (f new-state)
      (let ((old-state state))
        (set! state new-state)
        old-state
        ))
    f))
