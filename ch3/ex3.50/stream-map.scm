(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams)) ;; Applies proc to the first argument of each argstream.
      (apply stream-map ;; The stream-cdr is not evaluated until forced
	     (cons proc (map stream-cdr argstreams)))))) ;; When the stream-cdr is forced the stream-map procedure will be called
							 ;; on the rest of the stream, which will just be the next stream-car.
