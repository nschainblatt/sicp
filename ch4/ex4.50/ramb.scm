(cd "../amb-eval")
(load "amb-eval.scm")

;; Task:

;; a.
;; Update analyze-amb to choose a choice at random, instead of sequentially moving through options.
;; Make sure not to choose the same path twice.

;; We can choose an index of the list at random.
;; However we need a way to remove this option from further use.
;; We can do this by creating an iterator that builds up a new list as we are going to the random index.
;; Then it returns a new list without the current option.

;; b.
;; Use the changes to help Alyssa in ex4.49 (generate creative non recursive sentences).

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	  (fail)
	  (let* ((rpair (random-choice-pair choices))
		 (random-choice (car rpair))
		 (rest-choices (cdr rpair)))
	    (random-choice env
			   succeed
			   (lambda ()
			     (try-next rest-choices))))))
      (try-next cprocs))))

;; Returns a pair of the random choice, and the remaining choices without the chosen one.
(define (random-choice-pair choices)
  (let ((target (random-index choices)))
    (define (iter index pre rest)
      (if (= index target)
	(cons (car rest) (append pre (cdr rest)))
	(iter (inc index) (append pre (list (car rest))) (cdr rest))))
    (iter 0 '() choices)))

;; Returns a random index in range of seq.
(define (random-index seq)
  (random (length seq)))

(define (inc x)
  (+ x 1))

;; ---
;; ex4.49 to copy into the driver loop:

;; Returns one of the words in the word list wrapped in it's type.
;; We have to reattach the type for subsequent choices since it's always the first element of a word-list,
;; and we want to know the type.
(define (generate-word word-list)
  (if (null? (cdr word-list))
    (amb)
    (let ((type (car word-list))
	  (word (cadr word-list))
	  (rest (cddr word-list)))
      (amb (list type word) (generate-word (cons type rest))))))


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

(define the-global-environment (setup-environment))
(driver-loop)
