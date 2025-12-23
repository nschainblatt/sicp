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

;; Update:

; It was actually much simpler to implement after reviewing online.
; I shouldnt have implemented recursion myself and should have allowed the syste to do it for me by asserting
; logical deductions:

; From the book:
; We can regard a rule as a kind of logical implication: If an assignment
; of values to paern variables satisfies the body, then it satisfies the con-
; clusion. Consequently, we can regard the query language as having the
; ability to perform logical deductions based upon the rules. As an exam-
; ple, consider the append operation described at the beginning of Section
; 4.4. As we said, append can be characterized by the following two rules:
; • For any list y, the empty list and y append to form y.
; • For any u, v, y, and z, (cons u v) and y append to form (cons u z) if v and y append to form z.

; To express this in our query language, we define two rules for a relation
(append-to-form x y z)
; which we can interpret to mean “x and y append to form z”:
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;; We can use these assumptions to create a rule using logical deductions:

; The last pair of a single item list is the single item list.
(rule (last-pair (?x) (?x)))

; The last pair of a list is ?y if the last pair of ?rest is also ?y.
(rule (last-pair (?car . ?rest) ?y)
      (last-pair ?rest ?y))

; For example: (last-pair (2 ?x) (3))
; Our rules logically deduce this using the second rule, recursively breaking down the problem until the first rule:
; (rule (last-pair (?x) (?x))) can be used, which returns the last pair back to the second rule, using it in the ?y variable.
