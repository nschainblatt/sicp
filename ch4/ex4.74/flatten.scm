; Exercise 4.74: Alyssa P. Hacker proposes to use a sim-
; pler version of stream-flatmap in negate, lisp-value,
; and find-assertions. She observes that the procedure that
; is mapped over the frame stream in these cases always pro-
; duces either the empty stream or a singleton stream, so no
; interleaving is needed when combining these streams.
; a. Fill in the missing expressions in Alyssa’s program.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map (lambda (single) (if (not (= (stream-length single) 1)) (error "not a singleton" single) (car single)))
	      (stream-filter (lambda (p) (not (null? p))) stream)))

; b. Does the query system’s behavior change if we change
; it in this way?

; I don't believe any behavior will change with this update. This is because the flattening procedures operate on a single stream.
; We would only need interleave if one of the nested streams could be infinite. Since the streams in the procedures we updated
; will only ever contain singletons or empty streams, we don't run the risk with this change.

; I have added an assertion to ensure that we catch any cases where we don't have a singleton.
