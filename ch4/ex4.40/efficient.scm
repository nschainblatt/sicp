;; a.
;; In the multiple dwelling problem, how many
;; sets of assignments are there of people to floors, both
;; before and after the requirement that floor assignments be
;; distinct?

;; Well there is only one answer to the original question where the people to floors must be distinct, with all the same floor
;; requirements as before:

;; Original
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; If we remove the distinct requirement, these answers are now possible:

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    ; (require (distinct? (list baker cooper fletcher miller smith))) --changed
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker) (list 'cooper cooper)
	  (list 'fletcher fletcher) (list 'miller miller)
	  (list 'smith smith))))

;; Original
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; New possibilities:
;; ((baker 3) (baker 2) (baker 4) (baker 5) (smith 1))
;; ((baker 3) (baker 2) (baker 4) (miller 5) (smith 1))
;; ((baker 3) (baker 2) (fletcher 4) (miller 5) (smith 1))
;; ((baker 3) (cooper 2) (cooper 4) (miller 5) (smith 1))
;; This could go on and on, simply by repeating people on floors they are allowed to be on given the requirements that remain.
;; We have not implemented 'amb' into the evaluator yet, so these would have to be done by hand.


;; b.
;; It is very inefficient to generate all possible assignments
;; of people to floors and then leave it to backtracking
;; to eliminate them. For example, most of the restrictions
;; depend on only one or two of the person-floor variables, and
;; can thus be imposed before floors have been selected for
;; all the people. Write and demonstrate a much more efficient
;; nondeterministic procedure that solves this problem
;; based upon generating only those possibilities that are not
;; already ruled out by previous restrictions. (Hint: This will
;; require a nest of let expressions.)

;; Solve each requirement incrementally with ambs interleaved with requires to not have to backtrack everyone when a requirement
;; fails.

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
	(miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
	(require (not (= (abs (- smith fletcher)) 1)))
	(let ((baker (amb 1 2 3 4)))
	  (require (distinct? (list baker cooper fletcher miller smith)))))))
  (list (list 'baker baker)
	(list 'cooper cooper)
	(list 'fletcher fletcher)
	(list 'miller miller)
	(list 'smith smith)))

;; This solution ensures the current persons floor is correct (by the their single person requirement, if any)
;; before moving onto the next person. If there is a multi-person requirement, it is placed in the let of the last person
;; being defined for that requirement. Finally, distinct is left for last because there is no point in requiring them to be
;; distinct if they don't meet their simpler, individual requirements.
;; This solution is more efficient because it reduces the total amount of backtracking. This is because each problem (usage of amb)
;; is separate for the most part (besides multi person requirements).
