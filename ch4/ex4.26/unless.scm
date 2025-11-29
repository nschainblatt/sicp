;; Ben is correct that we can implement unless as a special form to allow for lazy evaluation. We can achieve this by using
;; delay and force to modify the original expression to delay the evaluation of the usual and exception values.

;; Alyssa is also correct that this would merely be syntax, not a procedure that can be used in conjunction with higher order
;; procedures. This is because it is no longer treated as any other procedure. We have to handle this special form differently (lazily),
;; which does not follow the evaluation rules of other procedures. This prevents using them with higher order procedures because a
;; higher order procedure would expect it's procedure arguments to behave like strict-procedures. So if we passed unless as a
;; special form instead, it wouldn't have been fully evaluated and can alter the expected behavior.
;; We have seen in chapter 1 that applicative order and normal order evaluation can have different
;; outputs given the same function and inputs.

(define (unless->if exp)
  (let ((condition (unless-condition exp))
	(usual-value (unless-usual-value exp))
	(exceptional-value (unless-exceptional-value exp)))
    ;; We analyze in the execution procedure to prevent the usual or exceptional expressions from being evaluated before they
    ;; are needed. Using the if construct to use it's special form of not evaluating the consequent or predicate until needed.
    (lambda (env)
      (if ((analyze condition) env)
	((analyze exceptional-value) env)
	((analyze usual-value) env)))))


;; A use case when unless is implemented as a regular procedure would be one that works with applicative order evaluation since it is
;; expected to have the same evaluation rules of the language.

;; Occording to SICP, a special form is a kind of expression that has it's own rules for evaluation. So if unless was implemented as a
;; regular procedure, it would follow the same evaluation rules as the rest of the combinations.
;; So a good example for this would be an expression that returns a simple value, and that doesn't make any procedure calls:
(define (sign n)
  (unless (negative? n)
    '+
    '-))

;; A use case for a special form would be almost any example, including ones with recursive procedure calls or iteration.
;; This is because we would eliminate the chance of going into infinite recursion, which would happen if this used applicative
;; order evaluation (procedure implementation).
(define (for f start end)
  (unless (> start end)
    (begin (f start) (for f (+ start 1) end))
    'done))
