;; Exercise 4.78

;; Update the query system to run inside the AMB evaluator.
;; Allowing the query system to make use of non-deterministic
;; search and backtracking instead of the stream of frames.

;; Note that the query system will now run as a program within
;; the AMB evaluator.

;; So to use the query system. You must start the AMB evaluator,
;; feed it the procedures that make up the query system, add 
;; sample data, and then begin querying.

;; I will be making a query-system procedure to load all of this automatically
;; when you load this script.

;; Problems:

;; 1. Redesign the query language to run inside the AMB evaluator to make use of non-deterministic search and backtracking instead of using the stream of frames.
;;    - Read this script from within the AMB evaluator to get the system procedures
;;    - Update all usages of transformers (e.g. cons-stream) to use the AMB evaluator (point of this exercise and the fact that you can't use transformers as an expressions)
;;       - Why were streams used? To obtain all possible matches with easy ability to filter out mismatched.
;;       - How can we replace streams with AMB? AMB can be used instead of streams.
;;          We could simply place all database assertions and rules inside of the AMB expression.
;;          AMB will do the work for us by trying each choice until the first one that works. That will be returned and printed.
;;          Try again will allow the AMB to continue and try the next choice, until it gets another that matches. Prints that one and continues.
;;       - Are there any other AMB usages we need to replace the functionality of streams? Such as filtering or something similar?
;;          - I don't think so. We use the existing pattern matching functions (updated to no longer use streams) to check if the current query matches
;;            the current database records given from AMB. Filtering operations (not and lisp-value) will work the same way as before, except instead of eliminating
;;            the current frame, it will call the empty amb expression (amb) to force a backtrack to occur to try the next value.

;; 2. What is the subtle difference AMB brings that didn't exist with the stream of frames approach?

;; 2a. What are some examples of this?

;; ---

;;;;QUERY SYSTEM FROM SECTION 4.4.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;;Includes:
;;;;  -- supporting code from 4.1, chapter 3, and instructor's manual
;;;;  -- data base from Section 4.4.1 -- see microshaft-data-base below

;;;;This file can be loaded into Scheme as a whole.
;;;;In order to run the query system, the Scheme must support streams.

;;;;NB. PUT's are commented out and no top-level table is set up.
;;;;Instead use initialize-data-base (from manual), supplied in this file.


;;;SECTION 4.4.4.1
;;;The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           ;; [extra newline at end] (announce-output output-prompt)
           (newline)
           (display (instantiate q (qeval q '()) (lambda (v f)
                              (contract-question-mark v))))
           'done
           ))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


;;;SECTION 4.4.4.2
;;;The Evaluator

(define (qeval query frame)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame)
        (simple-query query frame))))

;;;Simple queries

(define (simple-query query-pattern frame)
  (amb (find-assertions query-pattern frame) (apply-rules query-pattern frame)))

;;;Compound queries

(define (conjoin conjuncts frame)
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame))))

;;(put 'and 'qeval conjoin)


;; Disjoin uses amb (an additional usage to the one that processes the internal queries) because according to the specification
;; in the book, or clauses are to be evaluated with the original frame independently of each other. This allows each clause to have it's own
;; output frame, having different values for the variables used to instantiate the original query.
;; Supplying try-again will go through all database assertions and rules that pass the first clause in the or. This first suite of try-agains hit the amb within
;; the simple-query processor. When those options are exhausted, the amb usage in this disjoin procedure tries the next clause, again trying each database entry
;; for this clause.
;; So the order is:
;; 1. Enter a disjoin query
;; 2. The first clause is attempted with all database entries, that frame is used to instantiate the original compound query.
;; 3. Inputting try-again will attempt subsequent database entries with the first clause.
;; 4. Once those options are exhausted, the next clause is tried in the same fashion.
;; Another example:
;; With negate, if the query produces values, we run (amb), which if the negation was used in a disjoin query
;; then we would try the next clause.
;; NOTE: there is no actual frame merging going on in the original query evaluator. This causes variables to remain unbound if they are not shared in
;; each of the clauses.
;; For example:
;; (or (job ?x ?y) (job ?x ?a))
;; Produces some results where non-shared variables are unbound in output:
;; (or (job (bitdiddle ben) ?y) (job (bitdiddle ben) (computer wizard)))
(define (disjoin disjuncts frame)
  (define (eval-all-disjuncts disjuncts)
    (if (empty-disjunction? disjuncts)
      (amb)
                                                         ;; in analyze-amb, maybe this is being analyzed as an application?
      (amb (qeval (first-disjunct disjuncts) frame) (eval-all-disjuncts (rest-disjuncts disjuncts)))))
  (eval-all-disjuncts disjuncts))

;;(put 'or 'qeval disjoin)

;;;Filters

;; Negate is slightly more complex as it needs to use (amb) when a match exists to signal this frame matches
;; the negate predicate.
;; However, because we must use qeval to know if a match exists, and qeval will call (amb) when
;; no match exists (basically the reverse of what we want) we have to wrap it in a if-fail.
;; Inside if-fail we have to assign a flag to know when a match is found, so we can call our of (amb)
;; in the last predicate to fetch a new frame to try.
;; The reason we have two occurrences of (amb) here is the first one inside if-fail is used to signal
;; the failure expression to be evaluated, and the second (amb) inside the failure expression is used
;; to get the next frame.
;; This is all to handle the fact that we need to know when qeval fails, so we can return the frame
;; passing the negate.
(define (negate operands frame)
  (let ((match-exists? false))
    (if-fail (begin (qeval (negated-query operands) frame) (permanent-set! match-exists? true) (amb))
             (if match-exists? (amb) frame))))

;;(put 'not 'qeval negate)

(define (lisp-value call frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
       frame
       (amb)))


;;(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) (setup-environment))
         (args exp)))

(define (always-true ignore frame) frame)

;;(put 'always-true 'qeval always-true)

;;;SECTION 4.4.4.3
;;;Finding Assertions by Pattern Matching

(define (get-amb-assertions)
  (define (iter assertions)
    (if (null? assertions)
      (amb)
      (amb (car assertions) (iter (cdr assertions)))))
  (iter (get-all-assertions)))

(define (find-assertions pattern frame)
    (check-an-assertion (get-amb-assertions) pattern frame))

(define (check-an-assertion assertion query-pat frame)
  (let ((match-result
         (pattern-match query-pat assertion frame)))
    (if (eq? match-result 'failed)
        (amb)
        match-result)))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;;;SECTION 4.4.4.4
;;;Rules and Unification

(define (get-amb-rules)
  (define (iter rules)
    (if (null? rules)
      (amb)
      (amb (car rules) (iter (cdr rules)))))
  (iter (get-all-rules)))

(define (apply-rules pattern frame)
  (apply-a-rule (get-amb-rules) pattern frame))

(define (apply-a-rule rule query-pattern frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        frame)))
      (if (eq? unify-result 'failed)
          (amb)
          (qeval (rule-body clean-rule)
                 unify-result)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; {\em ; ***}
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;;;SECTION 4.4.4.5
;;;Maintaining the Data Base

(define THE-ASSERTIONS '())

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-list (index-key-of pattern) 'assertion-list))

(define (get-list key1 key2)
  (let ((s (get key1 key2)))
    (if s s '())))

(define THE-RULES '())

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (append
   (get-list (index-key-of pattern) 'rule-list)
   (get-list '? 'rule-list)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-list
               (get-list key 'assertion-list)))
          (put key
               'assertion-list
               (cons assertion
                            current-assertion-list))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-list
                 (get-list key 'rule-list)))
            (put key
                 'rule-list
                 (cons rule
                              current-rule-list)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;;;SECTION 4.4.4.7
;;;Query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?" 
     (if (number? (cadr variable))
         (string-append (symbol->string (caddr variable))
                        "-"
                        (number->string (cadr variable)))
         (symbol->string (cadr variable))))))


;;;SECTION 4.4.4.8
;;;Frames and bindings
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))


(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))


;;;;From Section 4.1

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

;;;;Table support from Chapter 3, Section 3.3.3 (local tables)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;;; From instructor's manual

(define get '())

(define put '())

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS assertions)
           (set! THE-RULES rules)
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (deal-out rules-and-assertions '() '()))

;; Do following to reinit the data base from microshaft-data-base
;;  in Scheme (not in the query driver loop)
;; (initialize-data-base microshaft-data-base)

(define microshaft-data-base
  '(
;; from section 4.4.1
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(can-do-job (administration secretary)
            (administration big wheel))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
))
