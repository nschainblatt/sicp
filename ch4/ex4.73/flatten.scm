;; Exercise 4.73: Why does flatten-stream use delay explicitly? What would be wrong with defining it as follows:

;; New
(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave
      (stream-car stream)
      (flatten-stream (stream-cdr stream)))))

;; Original
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream))))))

;; Answer:
;; In the new version (broken) we would lead to an infinite loop if the stream was infinite.
;; This is because we access the stream-cdr of the stream in the argument to the recursive call to flatten-stream.
;; Due to applicative-order evaluation, we evaluate the arguments before applying the procedure. So this would lead to
;; an infinite loop if the stream argument was infinite.
