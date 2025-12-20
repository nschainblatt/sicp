;; Returns one of the words in the word list wrapped in it's type.
;; We have to reattach the type for subsequent choices since it's always the first element of a word-list,
;; and we want to know the type.
(define (generate-word word-list)
  (let ((type ((car word-list)))
	(word ((cadr word-list)))
	(rest ((caddr word-list))))
    (amb (list type word) (generate-word (cons type (cddr word-list))))))


;; These parsers were renamed to generators, with no functional different. Just a name change.
(define (generate-sentence)
  (list 'sentence (generate-noun-phrase) (generate-verb-phrase)))

(define (generate-simple-noun-phrase)
  (list 'simple-noun-phrase
	(generate-word articles)
	(generate-word nouns)))

(define (generate-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
	 (maybe-extend
	   (list 'noun-phrase
		 noun-phrase
		 (generate-prepositional-phrase)))))
  (maybe-extend (generate-simple-noun-phrase)))

(define (generate-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
	 (maybe-extend
	   (list 'verb-phrase
		 verb-phrase
		 (generate-prepositional-phrase)))))
  (maybe-extend (generate-word verbs)))

(define (generate-prepositional-phrase)
  (list 'prep-phrase
	(generate-word prepositions)
	(generate-noun-phrase)))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

;; Example sentences and how they work:

;; The sentences will always start off with the first option of each amb, which is how amb works (depth first search).

;; So with the word lists above, the first possible sentence is:

;; 1. the student studies

;; No prepositional phrases are used since we are only using the simple first choices within the amb for each word list (within parse word for each word list).

;; The second sentence would be created after inputting 'try-again'. The most recent amb (the one inside the generate-word-phrase - maybe-extend procedure)
;; would take the second choice instead, leading to sentence with the same verb, but now with a preposition.
;; The preposition generator has it's own usage of parse-word, with a new amb usage for the words in it's phrase.
;; It starts over with a new list of prepositions and nouns (which is why the same noun and article is reused).

;; 2. the student studies for the student

;; The next sentence goes back to the latest usage of amb within the latest noun generation, which attaches a preposition to the last noun we added (again, the word list restarts
;; since it's a new call to parse-word)

;; 3. the student studies for the student for the student

;; This recursion cycle would repeat, adding a preposition, which adds a noun, which adds a preposition.
