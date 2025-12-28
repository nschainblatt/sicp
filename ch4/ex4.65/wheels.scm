;; The reason Warbucks Oliver appears 4 times is because of how many people Ben Bitdiddle supervise:

(rule (wheel (Warbucks Oliver)
	     (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
		  (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))))

(rule (wheel (Warbucks Oliver)
	     (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
		  (supervisor (Fect Cy D) (Bitdiddle Ben)))))

(rule (wheel (Warbucks Oliver)
	     (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
		  (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))

(rule (wheel (Warbucks Oliver)
	     (and (supervisor (Scrooge Eben) (Warbucks Oliver))
		  (supervisor (Cratchet Robert) (Scrooge Eben)))))


;; This is because of the unification process

;; The initial rule in the database 
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
	   (supervisor ?x ?middle-manager)))

;; Query read as a rule because no assertions for wheel exist in the database.
(wheel ?who)

;; Variables are bound
(rule (wheel ?who)
      (and (supervisor ?middle-manager ?who)
	   (supervisor ?x ?middle-manager)))

;; The queries within the 'and' condition are processed sequentially, with the second query receiving the extended frames from the
;; first, where the ?middle-manager is defined.

;; Since ?x is not defined in the extension, it is filled with all possible people that meet the criteria. The reason for the
;; three extra occurrences of Oliver is due to ben, because the three people Ben supervises each appear in a new frame for ?x.

;; The final results then instantiate the initial query, producing 4 results for Oliver.
