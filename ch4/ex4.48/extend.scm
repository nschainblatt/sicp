;; Extend the grammar given above to handle more complex sentences.
;; For example, you could extend noun phrases and verb phrases to include adjectives and
;; adverbs, or you could handle compound sentences.

(define (adverbs '(very always often quickly so)))
(define (adjectives '(tall fast red blue slow)))

; (define (parse-simple-noun-phrase)
;   (list 'simple-noun-phrase
; 	(parse-word articles)
; 	(parse-word nouns)))
;
; (define (parse-adjective-phrase)
;   (define (maybe-extend adjective-phrase)
;     (amb adjective-phrase
; 	 (maybe-extend
; 	   (list 'adjective-phrase
; 		 adjective-phrase
; 		 (list 'simple-adjective-phrase (parse-word adjectives))))))
;   (maybe-extend (list 'simple-adjective-phrase (parse-word adjectives))))

;; article adverb adjective noun preposition

; (define (parse-noun-phrase)
;   (define (maybe-extend)
;     (amb (parse-simple-noun-phrase) ;; when this fails (if there is adjective or adverb, we go to next alternative to parse them)
; 	 (maybe-extend
; 	   (list 'noun-phrase
; 		 (parse-word articles) ;; only one article allowed, required always.
; 		 (list 'adjective-phrase (parse-adjective-phrase)) ;; any number of adjectives allowed
; 		 ;; Any number of nested prepositions, note that the adjective parser will only do one
; 		 ;; initially, causing this parser to fail (because next word may be another adjective) and backtrack to get the rest of the adjectives.
; 		 (parse-prepositional-phrase)))))
;   (maybe-extend))
;; ^^^ I actually don't think this solution will work, because if the first choice fails (parse-simple-noun-phrase) then it would
;; backtrack to the previous amb and try another path. If this is the first usage of amb, the program will fail and stop.


;; This next solution will parse each word of the noun phrase one at a time in different expressions within the amb.
;; This is required because there are optional words inbetween the required article and noun words.
;; This works by starting with the article word, returning just the article as the noun-phrase.
;; When the next parser calls, it will fail because the next word is not what it was expecting (it's an adverb, adjective, or noun)
;; Then we will back track here and attempt to parse the next word.
;; However, now that I think of it, this won't work either because if we attempted to parse an adverb next, and there is no adverb
;; this amb would fail again.
...

;; This next solution will use if statements to check the next word to see if it is one of the optional words (adverb or adjective)
;; It will use the same pattern of parsing only a word at a time.
;; It will follow the same pattern of backtracking to parse the next word when the next parser fails (it encounters in invalid word).
;; Note that if an adverb or adjective is not present, their will still be a phrase in the noun, it will just be empty.

;; This works by parsing the required article first, then proceeding to to the next parser (parse-noun-phrase just returns the article
;; initially). This causes the next parser (lets say verb) to immediately fail it's requirement because the next word is not a verb.
;; This causes us to return through backtracking to the 'amb' within the parse-noun-phrase's maybe-extend procedure.
;; We then take the second option, which handles all the new grammar types (adverbs and adjetives).
;; Both the adverb and adjective parsers do not enforce the next word to be it's type, it only parses the next word if it's the 
;; correct type. This is required as at the end of the noun phrase the noun is required, followed by an optional prepositional phrase.
;; TODO explain how adverb and adjective parsers greedily take the right infinite number of words.
; (define (parse-noun-phrase)
;   (define (maybe-extend noun-phrase)
;     (amb noun-phrase ;; required first word, always.
; 	 (maybe-extend
; 	   (list 'noun-phrase
; 		 (parse-adverb-phrase)
; 		 (parse-adjectives-phrase)
; 		 (parse-word nouns)
; 		 (parse-prepositional-phrase)))))
;   (maybe-extend (parse-word articles)))


;; I kept my previous solutions above to see how I got here.
;; Note that since adverbs and adjectives are not required in a noun phrase (like the propositional phrase) but is in the middle
;; of the phrase, I am including them always, it will just appear as an empty but tagged list (based on type of word).

;; How this works:
;; The simple-noun-phrase now includes the optional adverbs and adjectives (both can have any number before the noun).
;; The first option in the amb is the simple noun phrase.
;; For the sentence without a verb: the very smart professor
;; Would result in this structure:
; (sentence
;   (simple-noun-phrase (article the)
; 		      (adverb-phrase (adverb very))
; 		      (adjective-phrase (adjective smart))
; 		      (noun professor)))
;; It would look like this with when the adverb and adjective are missing

;; the professor
; (sentence
;   (simple-noun-phrase (article the)
; 		      (adverb-phrase )
; 		      (adjective-phrase )
; 		      (noun professor)))

;; This was only the first option in the amb, if the noun phrase has a preposition to parse, it will happen next.
;; The first option would return as the value from the parse-noun-phrase procedure.
;; The next parser (the verb parser) attempts to parse the next word and fails (because it's a proposition, not a verb)
;; so we backtrack to the second option of the amb within parse-noun-phrase, and attempt to parse the proposition.
;; We succeed and return a nested noun phrase with the proposition inside:

;; the very smart professor in the class
; (sentence
;   (noun-phrase
;     (simple-noun-phrase (article the)
; 			(adverb-phrase (adverb very))
; 			(adjective-phrase (adjective smart))
; 			(noun professor))
;     (prep-phrase
;       (prep in)
;       (simple-noun-phrase (article the) (noun class)))))

;; then regular verb processing could continue.

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
	(parse-word articles)
	(parse-adverb-phrase)
	(parse-adjectives-phrase)
	(parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
	 (maybe-extend
	   (list 'noun-phrase
		 noun-phrase
		 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun)))

(define (parse-adverb-phrase)
  (let ((phrase (list 'adverb-phrase)))
    (if (is-next-word-of-word-list? adverbs)
      (append phrase (parse-word adverbs)))))

(define (parse-adjective-phrase)
  (let ((phrase (list 'adjective-phrase)))
    (if (is-next-word-of-word-list? adjectives)
      (append phrase (parse-word adjectives)))))

(define (is-next-word-of-word-list? word-list)
  (memq (car *unparsed*) (cdr word-list)))

;; Parsing verbs with adverbs is slightly different than for nouns.
;; This is because adverbs can go before and after the verb, while nouns are just before.

;; This is the structure, the adverb lists will be empty in the absence of adverbs

;; the professor professionally lectures loudly to the student with the cat
; (sentence
;   (simple-noun-phrase (article the)
; 		      (adverb-phrase )
; 		      (adjective-phrase )
; 		      (noun professor))
;   (verb-phrase
;     (verb-phrase
;       (adverb-phrase (adverb professionally))
;       (verb lectures)
;       (adverb-phrase (adverb loudly)))
;     (prep-phrase
;       (prep to)
;       (noun-phrase
; 	(simple-noun-phrase (article the) (noun student))
; 	(prep-phrase
; 	  (prep with)
; 	  (simple-noun-phrase (article the) (noun cat)))))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
	 (maybe-extend
	   (list 'verb-phrase
		 verb-phrase
		 (parse-prepositional-phrase)))))
  (maybe-extend (list 'verb-phrase
		      (parse-adverb-phrase) ;;optional
		      (parse-word verbs)
		      (parse-adverb-phrase)))) ;;optional
