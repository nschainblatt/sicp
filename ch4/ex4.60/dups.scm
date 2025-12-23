;; The reason the same people appear in a pair twice, is because we ensure that the result is not the same person,
;; but we do not treat order the same. Meaning two sets of the same values will be different if they are in different
;; orders.

;; To fix this we can implement logic enforce the same people cannot be in two results no matter the order.

;; Original rule, allowing duplication:
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))))

;; Updated rule v1:
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))
	   (not (lives-near ?person2 ?person1)))) ;; Ensure there isn't already a result with the same people in the reversed order.
;; This rule would lead to infinite recursion.

;; Updated rule v2:
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))

	   (lives-near ?person-3 ?person-4)
	   (and (not (same ?person-1 ?person-4)) ;; Ensure there isn't a result with the same people in reverse order.
		(not (same ?person-2 ?person-3)))))
;; This rule would not have any results, since each pair would invalidate the other, leaving both out.

;; My answer is no, it is not possible to eliminate possible duplicate results in a single rule. We would
;; need an outer rule to filter out the results of this rule.

;; Wrapping rule, utilizing the conditions from before:
(rule (lives-near-without-duplication ?person-1 ?person-2)
      (and (lives-near ?person-1 ?person-2)
	   (lives-near ?person-3 ?person-4)
	   (and (not (same ?person-1 ?person-4)) ;; Remove the results that have the same people involved just in reverse order.
		(not (same ?person-2 ?person-3))

		(not (same ?person-1 ?person-3)) ;; Ensure we don't have two ACTUAL normal order duplicated because we have
						 ;; two result sets because of the two calls to lives-near.
		(not (same ?person-2 ?person-4)))))

;; This rule will eliminate duplication based on order.
;; A wrapping rule is used to eliminate infinite recursion.

;; Example incremental output during the rule:

;; 1. The two lives-near calls causing complete duplicates and reverse order duplicates:
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

;; 2. Initial filtering, removal of reverse order duplicates:
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Hacker Alyssa P) (Fect Cy D))

;; 3. Last filtering step, removal of actual duplicates:
(lives-near (Hacker Alyssa P) (Fect Cy D))


;; We had to fetch two duplicate sets from 'lives-near' in order to have two sets of variables to manipulate to filter out
;; all duplicate data. If we had the ability to do direct filtering on the output, this answer could have been much simpler.
;; I would imagine we will learn about this feature later on in our query language evaluator.
