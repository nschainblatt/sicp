;; a.
;; Ben realized that since queries can have duplicate results, his accumulation function would have duplicate values
;; in a sum, average, or maximum function.

;; b.
;; To work around this, you could use a distinct function to filter our duplicate results. To do this we would require
;; a use of a unique key variable (such as a persons name or id).

;; So for this example from before:
(sum ?amount (and (job ?x (computer programmer))
		  (salary ?x ?amount)))
;; We would just need to implement the distinct functionality within each aggregate function (behind the scenes, in the implementation
;; of the evaluator).
;; The unique key variable here would be ?x, the individual people we are summing their salaries for programmers.

;; I believe we could check the results before instantiating the variables, removing duplicates, before we sum any values.

;; This is assuming the book is not looking for an answer that would eliminate all duplicate values from all queries.
