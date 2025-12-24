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

;; Update
;;
;; The 'or' condition is evaluated in parallel, merging their frames at the end.
;;
;; Due to this information, both the queries within the 'or' are evaluated at the same time.
;; They have their own stream of frames with bindings, so the variable ?boss used in both queries of the 'or' condition
;; are separate until they are merged at the end.
;;
;; Since both branches of the 'or' are always evaluated, and the variables of ?boss are separate (in different frames only merged
;; at the end which never happens due to the loop), we enter infinite recursion. This is because the ?middle-manager variable
;; in the initial part of the 'and' condition as well as the '?boss' variables are not assigned to any values, so they are extended
;; to try every person that fits the criteria. To validate the criteria, a recursive call to outranked-by is made for
;; every person filling those variables. This leads to the exact same situation, leading to an infinite loop.
