;; Original
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
	 (maybe-extend
	   (list 'verb-phrase
		 verb-phrase
		 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


;; Louis's New 'Straightforward' Solution
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
	     (parse-verb-phrase)
	     (parse-prepositional-phrase))))
;; a.
;; Does this work?
;; I don't believe this would work, because the second path leads to recursive calls to 'amb', where the verb phrase is re-parsed,
;; where the original uses the already parsed verb-phrase from the first path. This new version would attempt to parse the next
;; word as a verb, when we have already parsed the verb word.
;; If the next word was a proposition, this parsing would fail because it's not a verb.

;; Example sentence: the professor lectures to the student.

;; Skip to the verb "lectures", we have parsed the verb using the first option in the parse-verb-phrase's 'amb' usage.
;; The requirement on the input being null fails, so be backtrack to this 'amb' (the most recent usage) and take the next path.
;; This is the non-simple verb phrase which contains a proposition. To parse this, Louis's method called parse-verb-phrase recursively,
;; which has another 'amb' usage whose first path is to parse a verb again. This would break because the next word is not a verb,
;; it's a proposition.

;; I am still confused on which kind of backtracking actually puts words back into the input, whether it's with any backtracking
;; or just with manual usage of 'try-again'.
;; If we don't replace the word except for the 'try-again' is used, my answer should be correct.
;;
;; However, if we replace the word after any backtracking, that would mean we put the verb back into the input, allowing the recursive
;; call to parse-verb-phrase to successfully re-parse the verb. However, we would still run into the issue of the input being non-null,
;; because we still have the proposition to parse. This would create a cycle of replacing the verb and re-parsing it, and then failing
;; to parse the proposition.


;; b.
;; Does the program’s behavior change if we interchange the order of expressions in the amb?

;; With expressions in 'amb' interchanged
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
	     (parse-prepositional-phrase)
	     (parse-verb-phrase))))

;; If we use the same sentence: the professor lectures to the student.

;; No, this would still fail as the recursive parse-verb-phrase make a call to parse-word, which requires the input to be non-null.
;; The input would be null because the prepositional phrase is the last word in this sentence.
