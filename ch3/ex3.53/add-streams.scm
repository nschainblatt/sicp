;; Stream s is an infinite stream whose subsequent stream-cdr'ss produce the values:
;; 1 2 4 8 16 32 64 128 and so on...

;; This works because the car of the stream s starts out as 1. Every subsequent stream-cdr applied to the stream
;; computes the sum the elements of stream s against itself using stream-map.

;; Example of how the first stream-cdr is computed:
;; The stream-car of s is 1
;; The stream-cdr of s is the expression: (stream-map + s s)
;; When evaluated we get the stream (2, ...), which is placed as the second element of stream s.
;; The third element of s is computed by the promise following element 2. The contents of that promise is to produce the sum of the
;; next stream-cdr of stream s, which is 2. Resulting in the third element of s to be (4, ...).

(define s (cons-stream 1 (add-streams s s)))
