;; Exercise 4.71: Louis Reasoner wonders why the simple-
;; query and disjoin procedures (Section 4.4.4.2) are imple-
;; mented using explicit delay operations, rather than being
;; defined as follows:

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append
	(find-assertions query-pattern frame)
	(apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave
      (qeval (first-disjunct disjuncts)
	     frame-stream)
      (disjoin (rest-disjuncts disjuncts)
	       frame-stream))))
(put 'or 'qeval disjoin)

;; Can you give examples of queries where these simpler definitions would lead to undesirable behavior?

;; The reason the simple-query and disjoin procedures use 'delay' explicitely is to delay the
;; second stream arguments to procedures 'stream-append-delayed' and 'interleave-delayed'.

;; In the delayed versions of simple-query, the rules are not fetched or matched until they are needed.
;; With Louis's version, the rules are fetched from the database immediately. However, they are still in stream form,
;; so just the stream-car would be evaluated. However, since we do this for every frame, we would be loading the first
;; element of every rule stream for each frame. We have seen rules that may be infinite in the past, so this delay allows
;; the user to at least see the assertions present, before seeing any rules that may be infinite. I don't really like
;; this because it seems to be patching bad practices (rules that run infinitely). Instead there should be validation to prevent
;; adding rules to the database that cause these loops.
