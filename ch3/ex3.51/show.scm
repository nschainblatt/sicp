(define (show x)
  (display-line x)
  x)


(define (display-line x)
  (newline) (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
    '()
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))


(define x
  (stream-map show
	      (stream-enumerate-interval 0 10)))

;; Since x is a stream, 0 was already shown because streams always have their first car available.
;; When the 5th ref of the stream is accessed, 1-5 is printed because those have to be loaded from the map.
;; When the 7th ref of the stream is accessed, only 6 and 7 is printed because 0-5 was already loaded and is not called with show
;; within stream-map.
