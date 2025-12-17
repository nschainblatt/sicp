;; With the grammar given above, the following sentence can be parsed in five different ways:

;;NOTE: “The professor lectures to the student in the class with the cat.”

;; Give the five parses and explain the differences in shades of meaning among them.

;; 1. The professor lectures the student. The professor is with the cat.
;;    The student is in the class.
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

;; 2. The professor lectures the student.
;;    The student is in the class with the cat.
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
	    (noun-phrase
	      (simple-noun-phrase (article the) (noun class))
	      (prep-phrase
		(prep with)
		(simple-noun-phrase (article the) (noun cat))))))))))

;; 3. The professor lectures the student. The professor is in the class.
;;    The student is with the cat

;; --- START

;; 4. The professor lectures the student. The professor is in the class. The professor is with the cat.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (simple-noun-phrase (article the) (noun student))))
      (prep-phrase
	(prep in)
	(simple-noun-phrase (article the) (noun class))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat)))))


;; 5. With the cat is attached to the professor noun.
(sentence
  (noun-phrase
    (simple-noun-phrase (article the) (noun professor))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat))))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (simple-noun-phrase (article the) (noun student))))
      (prep-phrase
	(prep in)
	(simple-noun-phrase (article the) (noun class))))))

;; 6.
(sentence
  (noun-phrase
    (simple-noun-phrase (article the) (noun professor))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat))))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (simple-noun-phrase (article the) (noun student))))
      (prep-phrase
	(prep in)
	(simple-noun-phrase (article the) (noun class))))))


;; Starting over

;;NOTE: “The professor lectures to the student in the class with the cat.”

;; 1. Reads normally up to the very lectures. The null requirement fails because there is more input to parse, the most
;; recent amb is tried again, which is in the verb parser. It attached the next word which is a prep-phrase to the verb.
;; This happens another time. Then the input is empty.
;;
;; The professor lectures to the student. The professor is in the class while lecturing. The professor is with the cat while
;; lecturing.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (simple-noun-phrase (article the) (noun student))))
      (prep-phrase
	(prep in)
	(simple-noun-phrase (article the) (noun class))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat)))))

;; 2. To get the second version, we input try-again into the amb evaluator. This will backtrack to the most recent amb and
;; take a new path (it will also place the latest word back in the input to read, undoing the previous set! operation).
;; I am assuming that when we 'try-again' that we actually go back to the second to latest amb, which is inside the verb parser, where
;; the actual most recent amb is inside the noun parser apart of the last prep parsing. I assume this because if we were to feed
;; 'with the cat' back into the input, and return to the noun amb, we would simply repeat the same pred phrase over and over.

;; NOTE: UPDATE: Remember that we are using APPLICATIVE ORDER EVALUATION. So what may appear to be the latest usage of amb may not be.
;; The latest usage of amb may be a call higher up the stack, since the arguments are evaluated before amb is applied, the inner usages
;; of amb aren't necessarily the latest ones, it will likely be the outer ones.


;;NOTE: “The professor lectures to the student in the class with the cat.”
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
	(verb lectures)
	(prep-phrase
	  (prep to)
	  (simple-noun-phrase (article the) (noun student))))
      (prep-phrase
	(prep in)
	(simple-noun-phrase (article the) (noun class))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat)))))
