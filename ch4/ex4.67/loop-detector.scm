;; To detect an infinite happening in the query system using patterns and frames
;; we need to check the following:
;;
;; - Have we used this rule before (pattern) with the same variable values (frame)?
;;       Use the current frame for argument variable values + history of rule names used since entering this function

;; When calling a rule in the query evaluator, we must keep track of all rules called locally (within) this outer rule.
;;
;; The check is made once we have found the rule from our internal state.
;; If we have already called this rule with the same argument values, all within the outer rule, we deem it as a loop.
;; The history resets whenever we leave the rule.

;; For example:
;; Rule with loop
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
	  (and (outranked-by ?middle-manager ?boss)
	       (supervisor ?staff-person
			   ?middle-manager))))

;; The history begins when the first call to outranked-by is made.
;; Since the 'or' condition evaluates it's options in parallel, an immediate recursive call is made to outranked-by.
;; We record all rule calls made within any rule, so we record this one, along with the variable values (using the current frame).
;; In this recursive call, the same situation occurs, we record it if it is using different values for the variables, or throw
;; an error (for now, perhaps we can introduce recovery behavior later) if they are the same as a call made before.
;; This would happen when the ?middle-manager and the ?boss are the same people in a previous call to outranked-by.
;; Note that this is all local to the evaluation of the initial call to outranked-by by some external code.

;; History Requirements
;;  - Patterns of rules made since calling a rule
;;  - All frames creating by those rule calls (values for the variables used)
