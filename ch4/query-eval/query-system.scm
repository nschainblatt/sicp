(cd "..")
(load "procedure-table.scm")
(cd "amb-eval")
(load "eval.scm")

;; Required for 'get' and 'put' methods on table instance.
(define eval-procedure-table (make-table))

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
	    (display-stream
	      (stream-map
		(lambda (frame)
		  (instantiate
		    q
		    frame
		    (lambda (v f)
		      (contract-question-mark v))))
		(qeval q (singleton-stream '()))))
	    (query-driver-loop)))))

(define (display-line x)
  (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s))

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

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
      (qproc (contents query) frame-stream)
      (simple-query query frame-stream))))

(define (println . args)
  (newline)
  (define (init a)
    (if (null? a)
      'done
      (begin
	(if (stream-pair? (car a))
	  (begin (display "STREAM: ")
		 (display-stream (car a)))
	  (display (car a)))
	(display " ") (init (cdr a)))))
  (init args))


(define (uniquely-asserted unique-query-contents frame-stream)
  (let ((results (stream-flatmap
		   (lambda (frame) 
		     (let ((eval-results (qeval (car unique-query-contents) (singleton-stream frame))))
		       (if (= (stream-length eval-results) 1)
			 eval-results
			 the-empty-stream)))
		   frame-stream)))
    results))
(put 'unique 'qeval uniquely-asserted)

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
	(find-assertions query-pattern frame)
	(delay (apply-rules query-pattern frame))))
    frame-stream))

;; problems:

;; 1. Process the two 'and' clauses separately. just start with two for now, do more than two later.

;; 2. Look for all pairs of compatible frames.
;;    A comparible pair frames are two that if they have the same variable symbol, they have the save value. A compatible
;;    frame can be one that has no shared variables.
;;    When a compatible pair of frames are found, they are merged:
;;    ((a 1) (b 2) (c 3)) + ((a 1) (d 4) (e 5)) = ((a 1) (b 2) (c 3) (d 4) (e 5))
;;    Note: all shared variable symbols in two frames must be equal for them to be compatible.

;; 3. Design a procedure that accepts two frames and determines if they are compatible.
;;    If they are compatible, a single new merged frame is returned as output
;;    If they are incompatible, 'failed is returned.

;; Returns true if both frames supplied are compatible.
;; A compatible pair of frames are two frames that if they have any shared variable symbols, they have the save values.
;; A compatible frame can be one that has no shared variables.
;; If these were stored in a hash-table instead of a sequence of pairs, I would be able to do more efficient comparisons of variables.
;; Implementation Details:
(define (if-compatible-merge frame1 frame2)
  (define (iter f1 f2 result)
    (if (null? f1)
      result
      (let ((f1-binding (car f1))
	    (f2-binding-seq (binding-in-frame (binding-variable f1-binding) f2))) ;; result from assoc (rest of list after finding binding with binding as car).
	(if f2-binding-seq
	  (if (equal? (binding-value f1-binding) (car f2-binding-seq))
	    (if-compatible-merge (cdr f1) f2 (cons f1-binding result)) ;; found binding and values are equal, compatible.
	    #f) ;; found binding, but values were different, not compatible.
	  ;;  ^^ NOTE: this may be a bug, since #f will get lost in result. Can probably filter out the #f like they do with 'failed in pattern matching.
	  (if-compatible-merge (cdr f1) f2 (cons f1-binding result)))))) ;; didn't find binding in f2, good to add to final frame.
  (cond ((null? frame1) frame2) ;; if frame1 is null return frame2 as we want every binding in frame2 since it's compatible.
	((null? frame2) frame1) ;; "
	(else (iter frame1 frame2 '()))))

;; 4. Design a procedure that calls the comparison procedure from problem #3 with every frame combination in the two streams of frames.
;;    This will have quadratic time complexity as we have to compare every single frame with every other frame. N*M where N is the size of the first
;;    stream of frames and M is the size of the second. This iterator is in charge of constructing the new output stream of frames. It filters out the 'failed
;;    merges from the successful one, keeping a single stream of frames that are compatible.

;; Returns a stream of frames that is the result of merging all compatible frames in each stream input argument.
(define (comparison frame-stream1 frame-stream2)
  ;; Compare each frame in f1-stream to each in f2-stream, for compatibility.
  ;; Build up a result that is a one dimensional stream of merged frames.
  (define (iter f1-stream f2-stream result)
    (if (null? f1-stream)
      result
      (let ((frame (stream-car f1-stream)))
	(define (compaterator frame-stream)
	  (if (null? frame-stream)
	    '()
	    (let ((frame-optional (if-compatible-merge frame (car frame-stream))))
	      (if frame-optional
		(stream-cons frame-optional result)))))
	(iter (stream-cdr f1-stream) f2-stream (compaterator f2-stream)))))

  ;; TODO: determine if this cond is necessary
  (cond ((null? frame-stream1) frame-stream2)
	((null? frame-stream2) frame-stream1)
	(else (iter frame-stream1 frame-stream2 '()))))

;; 5. Glue all of this together inside conjoin. Update conjoin to qeval each of the two clauses independently, both with the original frame-stream.
;;    Then with these two extended stream frames pass them to the comparison iterator from problem #4.
;;    Return the final stream of frames from the last call to the iterator as the return value for conjoin.

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
	     (qeval (first-conjunct conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null?
	    (qeval (negated-query operands)
		   (singleton-stream frame)))
	(singleton-stream frame)
	the-empty-stream))
    frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute
	    (instantiate
	      call
	      frame
	      (lambda (v f)
		(error "Unknown pat var: LISP-VALUE" v))))
	(singleton-stream frame)
	the-empty-stream))
    frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap
    (lambda (datum) (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
	  (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
      the-empty-stream
      (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
	((equal? pat dat) frame)
	((var? pat) (extend-if-consistent pat dat frame))
	((and (pair? pat) (pair? dat))
	 (pattern-match
	   (cdr pat)
	   (cdr dat)
	   (pattern-match (car pat) (car dat) frame)))
	(else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
      (pattern-match (binding-value binding) dat frame)
      (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
		    (apply-a-rule rule pattern frame))
		  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
				     (conclusion clean-rule)
				     query-frame)))
      (if (eq? unify-result 'failed)
	the-empty-stream
	(qeval (rule-body clean-rule)
	       (singleton-stream unify-result))))))

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
	((var? p2) (extend-if-possible p2 p1 frame)) ; ***
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
	    (unify-match (binding-value binding) val frame))
	  ((var? val) ; ***
	   (let ((binding (binding-in-frame val frame)))
	     (if binding
	       (unify-match
		 var (binding-value binding) frame)
	       (extend var val frame))))
	  ((depends-on? val var frame) ; ***
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

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
      (cons-stream assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream
	      (get-stream key 'assertion-stream)))
	(put key
	     'assertion-stream
	     (cons-stream
	       assertion
	       current-assertion-stream))))))
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
	(let ((current-rule-stream
		(get-stream key 'rule-stream)))
	  (put key
	       'rule-stream
	       (cons-stream rule
			    current-rule-stream)))))))
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (stream-append-delayed
	(stream-cdr s1)
	delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (interleave-delayed
	(force delayed-s2)
	(delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

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

(define (add-assertion-body exp) (car (contents exp)))

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
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

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

(define (var? exp) (tagged-list? exp '?))
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

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (run q)
  (cond ((assertion-to-be-added? q)
	 (add-rule-or-assertion! (add-assertion-body q)))
	(else
	  (display-stream
	    (stream-map
	      (lambda (frame)
		(instantiate
		  q
		  frame
		  (lambda (v f)
		    (contract-question-mark v))))
	      (qeval q (singleton-stream '())))))))

(define (run-many queries)
  (for-each run queries))

(define the-global-environment (setup-environment))

(run-many (list
	    '(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
	    '(assert! (job (Bitdiddle Ben) (computer wizard)))
	    '(assert! (salary (Bitdiddle Ben) 60000))
	    '(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
	    '(assert! (job (Hacker Alyssa P) (computer programmer)))
	    '(assert! (salary (Hacker Alyssa P) 40000))
	    '(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
	    '(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
	    '(assert! (job (Fect Cy D) (computer programmer)))
	    '(assert! (salary (Fect Cy D) 35000))
	    '(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
	    '(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
	    '(assert! (job (Tweakit Lem E) (computer technician)))
	    '(assert! (salary (Tweakit Lem E) 25000))
	    '(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
	    '(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
	    '(assert! (job (Reasoner Louis) (computer programmer trainee)))
	    '(assert! (salary (Reasoner Louis) 30000))
	    '(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
	    '(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
	    '(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
	    '(assert! (job (Warbucks Oliver) (administration big wheel)))
	    '(assert! (salary (Warbucks Oliver) 150000))
	    '(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
	    '(assert! (job (Scrooge Eben) (accounting chief accountant)))
	    '(assert! (salary (Scrooge Eben) 75000))
	    '(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
	    '(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
	    '(assert! (job (Cratchet Robert) (accounting scrivener)))
	    '(assert! (salary (Cratchet Robert) 18000))
	    '(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))
	    '(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
	    '(assert! (job (Aull DeWitt) (administration secretary)))
	    '(assert! (salary (Aull DeWitt) 25000))
	    '(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))
	    '(assert! (can-do-job (computer wizard) (computer programmer)))
	    '(assert! (can-do-job (computer wizard) (computer technician)))
	    '(assert! (can-do-job (computer programmer) (computer programmer trainee)))
	    '(assert! (can-do-job (administration secretary) (administration big wheel)))
	    ))

(query-driver-loop)
