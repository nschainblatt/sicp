; Descendants
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)


; Formulate rules such as:
; “If S is the son of f, and f is the
; son of G, then S is the grandson of G” and “If W is the wife
; of M, and S is the son of W, then S is the son of M”


; If S is the son of f, and f is the son of G, then S is the grandson of G.
(rule (grandson-of ?gs ?gp)
      (son-of ?gs ?f)
      (son-of ?f ?gp))

; If W is the wife of M, and S is the son of W, then S is the son of M.
(rule (son-of ?s ?m)
      (wife-of ?w ?m)
      (son-of ?s ?w))

;; When the relationship is known, but not the people.
;; Find all occurrences of this great ... relationship in the database.
;; Recursively gather sons of the great ... Grandfather until we are just at grandson.
;; Then the right people are found.
(rule (great-rel (great . ?rel) ?x ?y) ;; ?rel is: great great great ... grandson
      (son-of ?sy ?y) ;; the son of the great great great ... grandfather
      (great-rel ?rel ?sy ?y)) ;; recursively get down to no more greats in the relationship, just grandson
(rule (great-rel (grandson) ?gs ?gp)
      (grandson-of ?gs ?gp)) ;; will return the grandson for the base case

;; When the relationship is unknown, but the people are known (the ?x and ?y variables)
;; Recursively build up a great ... list until ?x is the grandson of ?y.
(rule (?relationship ?x ?y)
      (and (grandson-of ?sy ?y) ;; Grab the immediate grandson of the Great ... Grandfather.
	   (or (and (same ?x ?sy) ;; Base case: The grandson of the current grandfather is the original grandson (great).
		    (append-to-form ?new-rel (grandson) ?relationship)) ;; When the base case is met, append the grandson portion.
	       (and (append-to-form (great) ?new-rel ?relationship) ;; For each time we have to get the next grandson, append great to the relationship.
		    (?relationship ?sy ?y))))) ;; Recursively build the relationship.

;; After reviewing my solution and the explanation behind 'or'. Parallel work will be done leading to potentially infinite recursion
;; or complex behavior.
