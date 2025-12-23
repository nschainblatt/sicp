; Define rules to implement the last-pair
; operation of Exercise 2.17, which returns a list containing
; the last element of a nonempty list.

; Check your rules on queries such as:
; (last-pair (3) ?x),         --> (last-pair (3) (3))
; (last-pair (1 2 3) ?x), and --> (last-pair (1 2 3) (3))
; (last-pair (2 ?x) (3)).     --> (last-pair (2 3) (3))

(rule (last-pair ?x ?y)
      ;; recursively get down to a list with one element. (maybe using next-to to get the next and next elements until () is next?)
      ;; use the append-to-form procedure to append the last element to an empty list

      (or (and (append-to-form (?last) () ?x) ;; last item is ?last (base case of the recursion)
	       (append-to-form (?last) () ?y)) ;; place the last element in a list and assign it to the output ?y variable.

	  (and ;; Otherwise, work down to the last element recursively.
	    (append-to-form (?curr) (?next . ?rest) ?x) ;; Get the rest of the elements
	    (last-pair (?next . ?rest) ?y))  ;; Pass the rest down to the next call as a decremented version of ?x to reach the base case.

	  (and ;; The next value of ?x is unknown (due to variable in list) and we know the last value pair (?y is filled)
	    (append-to-form ?y () ?y)
	    (last-pair ?y ?y)))) ;; Make the final recursive call with the last value in ?x.

; Do your rules work correctly on queries such as (last-pair ?x (3)) ?

; I believe so, we do not have a working query interpreter built to test it out.
; I am assuming that when a inner rule fails, we proceed to the next rule in the logical expressions until
; one works. I am also assuming that the final output keeps the original value of x and not the decremented version from the last
; recursive call.

; 1. If ?x is given, and ?y is left out, we work our way down the list within ?x until the last element. We append a list of
; this element with the empty list and assign it to ?y.

; 2. If part of ?x is missing, and ?y is given, we work our way as far down as possible until one of the ?x rules fails, then we
;    operate with ?y and make one final recursive call with the list of last element (whats in ?y).
