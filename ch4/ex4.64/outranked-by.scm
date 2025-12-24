;; Original Version
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
	  (and (supervisor ?staff-person ?middle-manager)
	       (outranked-by ?middle-manager ?boss))))

;; Louis's New Version (infinite loop)
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
	  (and (outranked-by ?middle-manager ?boss)
	       (supervisor ?staff-person
			   ?middle-manager))))

;; Why is there an infinite loop when checking who outranks Louis?

;; The swapping of the queries in the 'and' condition cause the infinite recursion.
;;
;; Since ?middle-manager wasn't defined by the supervisor of the ?staff-person like
;; in the original rule, a extended frame gets created for every person in the database.
;; We call outranked-by recursively with these frames.
;; Since a extended frame is created for every person in the database, the same person in ?staff-person
;; will eventually end up in one of these new frames, leading to the exact same behavior infinitely.
;;
;; In the scenarios where the ?staff-person never meets the first query in the 'or' condition, the rule will go into
;; an infinite recursion.
