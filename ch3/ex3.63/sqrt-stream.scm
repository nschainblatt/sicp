(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
		     (lambda (guess)
		       (sqrt-improve guess x))
		     (sqrt-stream x))))

;; a.
;; The reason this implementation is much less efficient is because accessing subsequent guesses makes a new recursive call to sqrt-stream.
;; Each recursive call in turn creates a new stream whose stream-cdr will make an additional recursive call. This repeats for every new guess.
;; With each guess accessed each recursion will have to process it's current guess (which has already been processed by a call higher up)
;; in order for the next guess to be made. So when accessing a guess further down the stream, the time will be much worse because each
;; stream produced by each recursion will have to process it's current guess and pass it to it's caller to use to compute it's current guess,
;; all the way down to the original call to sqrt-stream that will use it to produce the current guess.
;; The time complexity is exponential because the amount of work scales exponentially with the number of guesses, explained above.
;; The space complexity is also exponential for the same reason.


;; b.
;; If we removed memoization, this would not level the playing field in terms of efficiency. This is because there are many separate streams
;; being created with every recursive call, so they would all have their own memoization state, each recursion would still have to recompute
;; guesses already made by another call further up.
;; To fix this, we would have to create a single stream and use that stream to pass to the stream-map, like so:

(define (sqrt-stream-improved x)
  (define s (cons-stream 1.0 (stream-map
			       (lambda (guess)
				 (sqrt-improve guess x))
			       s)))
  s)


(define s1 (sqrt-stream-improved 16))

;; You can see the speed improvement by accessing a very large stream-ref (say 5000) of the returned stream and comparing the time to Louis's answer and mine.
