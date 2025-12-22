;; Returns a choice which is a prime sum pair of one of each passed sequence.
(define (prime-sum-pair seq1 seq2)
  (let ((a (an-element-of seq1))
	(b (an-element-of seq2)))
    (require (prime? (+ a b)))
    (list a b)))

(let ((pairs '()))
  (if-fail
    (let ((p (prime-sum-pair '(1 3 5 8)
			     '(20 35 110))))
      (permanent-set! pairs (cons p pairs))
      (amb))
    pairs))

;; The result of evaluating this expression which uses both 'if-fail' and 'permanent-set!' special forms is:

; ((3 23))

;; Note that only one prime-sum-pair is included in the initial answer as we set once and then immediately fail.
;; We have to use 'try-again' to get the next answer.

;; Inputting try-again will generate a new prime sum pair, and place it at the beginning of the same list permanently:

;; ((3 110) (3 23))

;; This would continue until we have exhausted all possible choices in prime-sum-pair.
