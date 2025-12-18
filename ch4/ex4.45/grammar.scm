(parse '(the professor lectures to the student in the class with the cat))

;; I actually see 6 ways this sentence could be parsed.
;; 1. The professor lectures to the student. The student is in the class. The class is with the cat.
;; 1. The professor lectures to the student. The student is in the class. The student is with the cat.
;; 2. The professor lectures to the student. The professor is in the class. The professor is with the cat.
;; 3. The professor lectures to the student. The student is in the class. The professor is with the cat.
;; 4. The professor lectures to the student. The professor is in the class. The student is with the cat.
;; 5. The professor lectures to the student. The professor is in the class. The class is with the cat.
;; 6. The professor lectures to the student. The student is in the class. The student is with the cat.

;; I had trouble with this exercise, when I go to see other solutions online, it looks like everyone is using the amb evaluator
;; we havn't gotten access to yet in the book. This seems to void the point of the exercise, which is to use your current mental modal
;; of the amb evaluator to do the problem yourself.

;; I've had numerous pathways, however, with the actual behavior of the amb still left to discover in the book, it is hard to
;; manually generate the outcomes. I can of course just build the output based off of the permutations of the prepositions in the
;; input, however this is not the point of the exercise.

;; 1. Note: this description might be wrong: The first parse allows the last usage of 'amb' at the end of each word to greedily parse the next word repeatedly.
;;    What this means is after the preposition: "to the student", the prepositions following this one go under it recursively
;;    within it's noun phrase.
;;    This is because when the parser stops at the end of "student", the null? requirement fails in the sentence parser,
;;    causing the evaluator to backtrack to the latest usage of 'amb' to try the next option. This is within the noun-parser of the
;;    latest preposition phrase: "to the student", the noun being "student", the next choice is a non-simple noun phrase
;;    with the next preposition: "in the class" stored within. This happens again for "with the cat", then the parsing is complete.
;;    I went ahead and manually created these using what I believe to be all possible outcomes.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
	(simple-noun-phrase (article the) (noun student))
	(prep-phrase
	  (prep in)
	  (noun-phrase
	    (simple-noun-phrase (article the) (noun class))
	    (prep-phrase
	      (prep with)
	      (simple-noun-phrase (article the) (noun cat)))))))))

;; 2.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
	(noun-phrase
	  (simple-noun-phrase (article the) (noun student))
	  (prep-phrase
	    (prep in)
	    (simple-noun-phrase (article the) (noun class))))
	(prep-phrase
	  (prep with)
	  (simple-noun-phrase (article the) (noun cat)))))))

;; 3.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
	(prep to)
	(noun-phrase
	  (simple-noun-phrase (article the) (noun student))
	  (prep-phrase
	    (prep in)
	    (simple-noun-phrase (article the) (noun class))))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat)))))

;; 4.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (noun-phrase
	    (simple-noun-phrase (article the) (noun student)))))
      (prep-phrase
	(prep in)
	(simple-noun-phrase (article the) (noun class))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat)))))

;; 5.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (noun-phrase
	    (simple-noun-phrase (article the) (noun student))
	    (prep-phrase
	      (prep with)
	      (simple-noun-phrase (article the) (noun cat)))))))
    (prep-phrase
      (prep in)
      (simple-noun-phrase (article the) (noun class)))))
