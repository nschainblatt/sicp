; Define a rule that says that person 1 can re-
; place person 2 if either person 1 does the same job as person
; 2 or someone who does person 1’s job can also do person 2’s
; job, and if person 1 and person 2 are not the same person.
; Using your rule, give queries that find the following

;; Can person1 be replaced by person2?
;; They can if:
(rule (can-be-replaced-by ?person1 ?person2)
      (and (not (same ?person1 ?person2)) ;; They are not the same person
	   (job ?person1 ?type1)
	   (job ?person2 ?type2)
	   (or (same ?type1 ?type2) ;; They have the same job
	       (can-do-job ?type2 ?type1)))) ;; Or person2's job can do person1's job

;; a. all people who can replace Cy D. Fect;
(can-be-replaced-by (Fect Cy D) ?person)

;; b. all people who can replace someone who is being paid more than they are, together with the two salaries
(and (salary ?person1 ?amount1)
     (salary ?person2 ?amount2)
     (list-value > $amount1 $amount2)
     (can-be-replaced-by $person1 $person2))
