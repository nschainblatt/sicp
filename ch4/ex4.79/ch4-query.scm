;; Exercise 4.79
;; 1. Implement for the query language a rule-application method that uses environments rather than renaming.
;;    Solution:
;;     - Rules now hold the environments they were defined in. This will either be a empty environment if defined globally, or
;;       the surrounding rule if defined within a rule.
;;     - Input variables are now tagged with a unique ID instead of the rule variables, this prevents name collisions between
;;       input variables and rule variables. This is required because the input variables must exist in a related frame in order
;;       to instantiate later, in other words, they cannot be in their own environment like with rules.
;;     - When applying a rule, we perform unification like normal, however we extract any non-relevant bindings from the result.
;;       relevant bindings are ones in the rule conclusion. So they contain the input variables (for instantiation), variable to
;;       variable bindings for the input to conclusion, as well as variable to value for those conclusion variables (if they exist
;;       in the surrounding frame.)
;;       This is all to mimick what happens when a procedure is applied in the regular evaluator: evaluate the arguments to bind their
;;       values to the procedure variables of the procedure definition.
;;     - We then apply the rule with this modified frame
;;     - Input variables are kept in the frames in order for instantiation to work, as this is completely different from applying
;;       a regular procedure in scheme.
;;
;;
;; 2.  See if you can build on your environment structure to create constructs in the query language for dealing
;;     with large systems, such as the rule analog of block-structured procedures.
;;     Solution:
;;      - Implement the ability to have internal rule definitions that are local to the rule that it is defined within.
;;      - These internal definitions will use assert!, they will not have to be within a compound query, so we have
;;        to convert our assumption that rule bodies are a single query, they are now going to be a sequence of queries to evaluate.
;;          - Variables within separate compound queries will not be shared unless they are in the same wrapping query.
;;          - It is assumed that the last query in the body will return a frame.
;;      - This would require the environment the outer rule contains to have a binding to these internal rules.
;;      - The outer rule body would exist in the database like normal, only when we apply this rule will the body be evaluated,
;;        which is where the internal rule definitions reside.
;;      - This requires adding a new check to qeval, we would need to separate the standard rule definitions from the internal
;;        ones so we don't add them to the global database. They should remain local to the rule they were defined in.
;;          - We could tag inner rule definitions with inner-rule, that way we can handle them differently.
;;          - Inside the qeval handler for inner-rules we will simply add the inner-rule to the frame passed in, then return that frame
;;      - With the inner-rule defined in the local frame, we can then call it from the rule body
;;      - We will have to update simple-query to have another option for checking for inner-rules, however, I will place this option in
;;        between the exiting assertions and apply-rule options, that way a inner rule has higher precdedence than the global rules.
;;      - Inside this new option, we search the frame for a matching inner-rule with that name
;;      - When found, we cal qeval with the current frame that should have the bindings for the variables used in the conclusion already
;;
;;       (assert! (rule (kinda-rich ?person)
;;                      (inner-rule (house-rich)
;;                                           (address ?person (Swellesley (Top Heap Road))))
;;                      (inner-rule (money-rich)
;;                                           (and (salary ?person ?amount)
;;                                                (lisp-value > ?person 50000)))
;;                      (or (house-rich ?person) (money-rich ?person))))
;;
;; 3. Can you relate any of this to the problem of making deductions in a context (e.g., “If I supposed that P were true,
;;    then I would be able to deduce A and B.”) as a method of problem solving? (This problem is open-ended. A good answer
;;    is probably worth a Ph.D.)

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
  ;; Rename the variables so we know the input variable differs from any rule variables that it may conflict with.
  (let ((q (rename-variables-in (query-syntax-process (read)))))
    (newline)
    (display output-prompt)
    ;; [extra newline at end] (announce-output output-prompt)
    (newline)
    (display (instantiate q (qeval q the-empty-frame) (lambda (v f)
                                                        (contract-question-mark v))))
    'done))

(define (rename-variables-in qexp)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk qexp)))

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
  (println "QEVAL" query)
  (cond ((assertion-to-be-added? query) (add-rule-or-assertion! (add-assertion-body query) frame) frame)
        ((inner-rule-to-be-added? query) (add-inner-rule! query frame) frame)
        (else
          (let ((qproc (get (type query) 'qeval)))
            (if qproc
              (qproc (contents query) frame)
              (simple-query query frame))))))

(define (inner-rule-to-be-added? statement)
  (tagged-list? statement 'inner-rule))

(define (add-inner-rule! rule-statement frame)
  (let ((rule (make-inner-rule rule-statement frame)))
    ;; TODO: check if rule already exists, if so we need to replace it
    (append! (bindings frame) (list (make-binding (car (conclusion rule)) rule)))))

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
      (begin (println "DISJOIN AMB FAILURE") (amb))
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
             (if match-exists? (begin (println "NEGATE AMB FAILURE") (amb)) frame))))

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
      (begin (println "ASSERTIONS AMB FAILURE") (amb))
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
      (begin (println "AMB RULES AMB FAILURE") (amb))
      (amb (car rules) (iter (cdr rules)))))
  (iter (get-all-rules)))

(define (apply-rules pattern frame)
  (apply-a-rule (get-amb-rules) pattern frame))

(define (apply-a-rule rule query-pattern frame)
  (let ((unify-result (unify-match query-pattern (conclusion rule) frame)))
    (if (eq? unify-result 'failed)
      (begin (println "FAILED TO APPLY RULE" rule unify-result) (amb))
      (let ((relevant-bindings (filter-irrelevant-bindings rule unify-result)))
        (qeval-sequence (rule-body rule) ;; We might need to qeval a sequence, since the rule-body may have other rule definitions now
               (extend-frame relevant-bindings (rule-frame rule)))))))

;; Evaluate each query in the sequence independently, similar to an or except we evaluate all queries in the sequence.
(define (qeval-sequence seq frame)
  (if (null? (cdr seq))
    ;; Return the frame of the last evaluated query
    (qeval (car seq) frame)
    (begin (qeval (car seq) frame) (qeval-sequence (cdr seq) frame))))

(define (filter-irrelevant-bindings rule frame)
  ;; Gather all variables in the rule
  ;; Go over every binding in the frame, only keeping those that are in the rule variable list
  ;; Return the resulting frame
  (define (get-rule-variables rule-conclusion)
    (if (null? rule-conclusion)
      '()
      (let ((exp (car rule-conclusion)))
        (if (var? exp)
          (cons exp (get-rule-variables (cdr rule-conclusion)))
          (get-rule-variables (cdr rule-conclusion))))))

  (let ((rule-variable-list (get-rule-variables (conclusion rule))))
    (define (frame-iter binds result-bindings)
      (if (null? binds)
        result-bindings
        (let ((variable (binding-variable (car binds))))
          (if (or (memq variable rule-variable-list) (input-variable? variable))
            (frame-iter (cdr binds) (cons (car binds) result-bindings))
            (frame-iter (cdr binds) result-bindings)))))
    (frame-iter (bindings frame) '())))


(define (input-variable? variable)
  (number? (cadr variable)))

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

(define (add-rule-or-assertion! assertion frame)
  (if (rule? assertion)
      (add-rule! assertion frame)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons assertion old-assertions))
    'ok))

(define (add-rule! rule-statement frame)
  (let ((rule (make-rule rule-statement frame)))
    (store-rule-in-index rule)
    (let ((old-rules THE-RULES))
      (set! THE-RULES (cons rule old-rules))
      'ok)))

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


(define (make-rule rule frame)
  ;; place the tag first, remove the tag from the passed rule.
  (list 'rule (cdr rule) frame))
(define (make-inner-rule rule frame)
  (list 'inner-rule (cdr rule) frame))
(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (caadr rule))
(define (rule-body rule)
  (println "RULE" rule)
  (if (null? (cdadr rule))
      '(always-true)
      (cdadr rule))) ;; if we decide to do a sequence, we will have to change this, we would remove the last car since
(define (rule-frame rule)
  (caddr rule))

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


;; UPDATE: frames now have parents (environments)
;; Search the current frame for the variable, if found return it.
;; If not found recursively search parent frames, returning false when not found
;; Frame structure:
;; empty frame: ()
;; frame with 1 parent: ((?x 1) ((?y 2)))
(define (binding-in-frame variable frame)
  (cond ((null? frame) false)
        ((null? (bindings frame)) false)
        (else (or (assoc variable (bindings frame)) (binding-in-frame variable (enclosing-frame frame))))))

(define (bindings frame)
  (car frame))
(define (enclosing-frame frame)
  (cdr frame))
(define (extend-frame bindings frame)
  (cons bindings frame))
(define the-empty-frame '(() ()))

(define (extend variable value frame)
  (cons (cons (make-binding variable value) (bindings frame)) (enclosing-frame frame)))


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
                    (let ((rule (make-rule s the-empty-frame)))
                    (store-rule-in-index rule)
                    (deal-out (cdr r-and-a)
                              (cons rule rules)
                              assertions)))
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
  (deal-out rules-and-assertions '() '())
  (println THE-RULES))

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
