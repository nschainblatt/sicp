;; a.
;; The reason Ben is right about the behavior of for-each working with the sequence this way is because the for each procedure uses
;; car to access the current element in the sequence. We installed car as a primitive in our language, and so we force the arguments
;; to a primitive (the expression) to evaluate whenever it is applied to a primitive.

;; b.
;; When we evaluate (p1 1) with the original version of eval-sequence, (1 2) is returned because the set! expression in the procedure
;; body is evaluated.

;; When we evaluate (p2 1) with the original version of eval-sequence, 1 is returned, because the set! expression is passed as an
;; argument to the next procedure, which gets delayed because of normal order evaluation, and when the sequence within the inner
;; procedure is evaluated, parameter e returns the thunk that contains the set! expression. So the set! expression never fully
;; evaluates, leaving x unmodified, returning 1.

;; When we evaluate (p1 1) with Cy's version of eval-sequence, each expression in the sequence is forced, so (1 2) is returned
;; because all expressions in the procedure fully evaluate.

;; (p2 1) returns (1 2) as well, as the thunk within parameter e is forced, evaluating the set! expression.

;; c.
;; The reason Cy's eval-sequence doesn't alter the outcome of the example in part 'a' is because the expression in the for-each
;; consequent is already forced because both display, newline, and car are primitives in the language.

;; d.
;; I think if you are knowingly using a normal order evaluator, then sequences should also follow those rules.
;; Forcing every expression but the last in a sequence is more complex than staying true to the lazy evaluation rules.
;; The developer should be aware of the consequences bringing assignment into a normal order evaluator create.
