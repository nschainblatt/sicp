;; The reason it is important for the amb evaluator to evaluate operands in left to right order is because of the potential
;; recursive usage of amb within other options.
;; For example parse-noun-phrase has this amb usage:
(amb noun-phrase
     (maybe-extend
       (list 'noun-phrase
	     noun-phrase
	     (parse-prepositional-phrase)))))
;; If we didn't evaluate from left to right, we would go into a recursion with further calls to amb.
;; This would break our backtracking, as we wouldn't go back to the right location to try another option.
;; For example if we just parsed a preposition, and we have to backtrack, we could potentially go to another 
;; amb usage that isn't expecting a preposition.
