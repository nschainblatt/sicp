;; Reverse the parser into a generator


;; It must follow the same grammatical structure


;; It will follow this pattern:

;; Build a sentence containing a noun and a verb in that order, ignoring optional words initially.
;; With subsequent 'try-again' input, different amb branches will be taken that will include the optional words (propositions
;; for both nouns and verbs).
