;; a.
;; What is the purpose of the let bindings
;; in the procedures add-assertion! and add-rule! ?

;; Answer:
;; After the set! expression, the let binding still contains the original values before the assignment.
;; These original values are used in the delayed stream-cdr of the assignment.
;; If we used the global variable THE-ASSERTIONS or THE-RULES instead of the let binding, we would introduce duplicate values
;; into the stream. This is because when the stream-cdr is evaluated, the assignment would have already completed, meaning
;; the stream-cdr when evaluated points to the updated global variable that already has the new stream-car, leading to the same
;; value appearing twice.

;; ---

;; b.
;; What would be wrong with the following implementation of add-assertion! ?
;; Hint: Recall the definition of the infinite stream of ones in Section 3.5.2:
;; (define ones (cons-stream 1 ones))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
    (cons-stream assertion THE-ASSERTIONS))
  'ok)

;; Answer:
;; As I said before this would place the assertion twice, in the stream-car and the first element in the stream-cdr, due to
;; delayed evaluation having the updated value after assignment.

;; Not only does this lead to repeating duplicate values, but it turns into an infinite stream. It does not keep the old
;; ending of the stream, and instead creates a cycle, circling back to the first element repeatedly.
