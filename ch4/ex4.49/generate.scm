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

;; Example sentences and how they work:
