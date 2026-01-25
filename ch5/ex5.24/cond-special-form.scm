;; Exercise 5.24: Implement cond as a new basic special form
;; without reducing it to if. You will have to construct a loop
;; that tests the predicates of successive cond clauses until you
;; find one that is true, and then use ev-sequence to evaluate
;; the actions of the clause.
;;
;; Steps:
;;
;; Note that cond if like if in that the first predicate in order that is true has it's consequent
;; evaluated and returned as the value for the entire cond expression. Clauses that follow this one
;; are not tried.
;;
;; So to evaluate a cond as a special form, we must do so similar to ev-if.
;; We loop through clauses until one is true. If we reach the else clause
;; we don't have a predicate to try, and we just evaluate the expression within
;; and return that value in register val.
;;
;; We also assume we have the syntax procedures available as primitives.

;; COND as special form in Scheme
(define (eval-cond exp)
  (define (eval-clauses clauses)
    (if (null? clauses)
      'false ; no else clause
      (let ((first (first-clause clauses))
	    (rest (rest-clauses clauses)))
	(if (cond-else-clause? first)
	  (if (null? rest)
	    (eval (sequence->exp (cond-actions first)))
	    (error "ELSE clause isn't last: COND->IF"
		   clauses))
	  (let ((pval (eval (cond-predicate first))))
	    (if pval
	      (eval (sequence->exp (cond-actions first)))
	      (eval-clauses rest)))))))
  (eval-clauses (cond-clauses exp)))

(define (first-clause clauses)
  (car clauses))
(define (rest-clauses clauses)
  (cdr clauses))

(define (true) #t)
(define (false) #f)
;; t => value2
(define t (cond ((false) 'value1)
		((true) 'value2))
		(else 'value3))
;; Error
(define t (cond ((false) 'value1)
		(else 'value3)
		((true) 'value2)))

;; Registers:
;; exp: cond expression, predicates, clause actions to evaluate.
;; unev: rest of cond clauses.
;; continue: location to return to with the value of the cond expression.
;; val: values of the cond expression or of it's sub expressions within the clauses.
;; env: the environment to evaluate the pieces of the cond expression in, unless there are procedure applications which are handled in their own extended environment.

ev-cond
  (assign unev (op cond-clauses) (reg exp))
  (goto (label ev-cond-loop))

ev-cond-loop
  (test (op null?) (reg unev))
  (branch (label no-else))
  (assign exp (op first-clause) (reg unev))                       ;; Assign the clause to try
  (assign unev (op rest-clauses) (reg unev))                      ;; Get the rest of the clauses to try later if current predicate isn't true
  (test (op cond-else-clause?) (reg exp))
  (branch (label handle-ev-cond-else))
  (goto (label ev-clause-predicate))

ev-clause-predicate
  (save env)                                                      ;; Save the environment the cond expression was defined in to be used for subsequent sub-expression evaluations in the cond.
  (save unev)                                                     ;; Save the clauses as eval-dispatch may overwrite the contents of the unev register.
  (save continue)                                                 ;; Save the location to return to with the value of the cond expression in register val (result of actions or false if no else)
  (save exp)                                                      ;; Save the first clause to handle later in ev-clause-did-predicate
  (assign exp (op cond-predicate) (reg exp))                      ;; Get the predicate to evaluate
  (assign continue (label ev-clause-did-predicate))               ;; Assign continue to where to go to handle the result of the predicates evaluation
  (goto (label eval-dispatch))                                    ;; Evaluate the predicate

ev-clause-did-predicate
  (restore exp)                                                   ;; Get the first clause back
  (restore continue)                                              ;; Get the location to return to if the predicate was true, or there are no more clauses to try
  (restore unev)                                                  ;; Get the remaining clauses to try
  (restore env)                                                   ;; Get the environment to evaluate the rest of the sub-expressions.
  (test (op true?) (reg val))                                     ;; Check the predicate
  (branch (label ev-cond-clause-actions))                         ;; Since the predicate was true, evaluate the actions, these will be the value of the cond expression
  (goto (label ev-cond-loop))                                     ;; Otherwise, try the next clause

ev-cond-clause-actions
  (assign exp (op cond-actions) (reg exp))                        ;; Get the cond actions from the current clause
  ;; No need to save any registers here as we are exiting this area, we will have the value of the cond in the val register, and eval-dispatch will use our current location in continue (the origial) to know where to return
  ;; to in order to use the new value.
  (goto (label eval-dispatch))                                    ;; Evaluate the actions, and return to caller with the actions value in register val

handle-ev-cond-else
  (test (op null?) (reg unev))                                    ;; Unev already has rest of clauses, should be null if we encountered an else
  (branch (label ev-cond-else))
  (perform (op error) (const "ELSE clause isn't last: COND->IF")  ;; Throw appropriate error with context of else clause and rest of clauses, this is a terminal operation, hence no jump after.
	              (reg exp)
		      (reg unev))

ev-cond-else
  (goto ev-cond-clause-actions)                                   ;; Evaluate the else clauses actions

no-else
  (assign val (const #f))                                         ;; Only location in ev-cond that assigns to val, eval-dispatch handles the results of clause actions.
  ;; No need to restore continue as it was restored in the previous loop (ev-clause-did-predicate), or if it was an empty cond expression the continue register still has the original location to return to.
  (goto (reg continue))                                           ;; Go back to the expression that needs the value of the cond.
