;; Exercise 5.18:
;;
;; a.
;; Modify the make-register procedure of Section 5.2.1 so that
;; registers can be traced. Registers should
;; accept messages that turn tracing on and off. When a reg-
;; ister is traced, assigning a value to the register should print
;; the name of the register, the old contents of the register,
;; and the new contents being assigned.
;;
;; b.
;; Extend the interface
;; to the machine model to permit you to turn tracing on and
;; off for designated machine registers.

(define (distinct seq)
  (define (iter rest result)
    (cond ((null? rest) result)
	  ((member (car rest) result) (iter (cdr rest) result))
	  (else (iter (cdr rest) (cons (car rest) result)))))
  (iter seq '()))

(define (my-sort seq fkey)
  (define (sort-iter rest result)
    (cond ((null? rest) result)
	  ((null? result) (sort-iter (cdr rest) (cons (car rest) result)))
	  (else (let ((instr-type (fkey (car rest)))
		      (curr-smallest-instr-type (fkey (car result))))
		  (if (symbol<=? instr-type curr-smallest-instr-type)
		    (sort-iter (cdr rest) (cons (car rest) result))
		    (sort-iter (cdr rest) (insert-instr-in-order (car rest) result)))))))
  (define (insert-instr-in-order instr seq)
    (let ((instr-type (fkey instr)))
      (define (insert-iter rest result)
	(if (null? rest)
	  (append result (list instr))
	  (let* ((curr-instr (car rest))
		 (curr-instr-type (fkey curr-instr)))
	    (if (symbol<=? instr-type curr-instr-type)
	      (append result (cons instr rest))
	      (insert-iter (cdr rest) (append result (list curr-instr)))))))
      (insert-iter seq '())))
  (sort-iter seq '()))

(define (symbol<=? x y)
  (or (eq? x y) (symbol<? x y)))

(define (println . args)
  (let* ((has-delimiter? (if (and (not (null? args)) (eq? (car args) 'delimiter))
			   (if (null? (cdr args))
			     (error "No delimiter supplied --PRINTLN")
			     #t)
			   #f))
	 (delimiter (if has-delimiter? (cadr args) " ")))
    (newline)
    (define (iter rest)
      (if (null? rest)
	'done
	(begin (display (car rest)) (display delimiter) (iter (cdr rest)))))
    (iter (if has-delimiter? (cddr args) args))))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-data-path) controller-text)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
	(tracing-enabled? #f))
    (define (set-register value)
      (if tracing-enabled?
	(begin (println 'delimiter "" "Register assignment: " name ". Old value: " contents ". New value: " value)))
      (set! contents value))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set) set-register)
	    ((eq? message 'enable-tracing) (set! tracing-enabled? #t))
	    ((eq? message 'disable-tracing) (set! tracing-enabled? #f))
	    (else
	      (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
	(number-pushes 0)
	(max-depth 0)
	(current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
	(error "Empty stack: POP")
	(let ((top (car s)))
	  (set! s (cdr s))
	  (set! current-depth (- current-depth 1))
	  top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
		     'maximum-depth '= max-depth
		     'current-depth '= current-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    ((eq? message 'print-statistics)
	     (print-statistics))
	    (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '())
	(data-path '())
	(instruction-execution-counter 0)
	(tracing-enabled? #f))
    (define (print-instruction-execution-counter)
      (newline)
      (display (list 'instructions-executed '= instruction-execution-counter)))
    (define (reset-instruction-execution-counter)
      (set! instruction-execution-counter 0))
    (let ((the-ops
	    (list (list 'initialize-stack
			(lambda () (stack 'initialize)))
		  (list 'print-stack-statistics
			(lambda () (stack 'print-statistics)))
		  (list 'print-instruction-execution-count
			(lambda () (print-instruction-execution-counter)))
		  (list 'reset-instruction-execution-counter
			(lambda () (reset-instruction-execution-counter)))))
	  (register-table
	    (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	  (error "Multiply defined register: " name)
	  (let ((new-reg (make-register name)))
	    (set! register-table (cons (list name new-reg) register-table))
	    new-reg)))
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	    (cadr val)
	    (allocate-register name)))) ;; return the new register object
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	    'done
	    (begin (if tracing-enabled? (trace-instruction (car insts)))
		   ((instruction-execution-proc (car insts)))
		   (set! instruction-execution-counter (+ instruction-execution-counter 1))
		   (execute)))))
      (define (trace-instruction inst)
	(newline)
	(if (instruction-label inst)
	  (begin (display "Label: ")
		 (display (instruction-label inst))
		 (newline)))
	(display "Trace: ") (display (instruction-text inst)))
      (define (make-data-path instructions)
	(define (all-instructions-filter)
	  (my-sort (distinct (filter (lambda (instr) (pair? instr)) instructions)) car))
	(define (register-label-filter)
	  (distinct (map (lambda (instr) (register-exp-reg (goto-dest instr)))
			 (filter (lambda (instr) (and (pair? instr) (eq? (car instr) 'goto) (register-exp? (goto-dest instr))))
				 instructions))))
	(define (register-save-restore-filter)
	  (distinct (map (lambda (instr) (stack-inst-reg-name instr))
			 (filter (lambda (instr) (and (pair? instr) (or (eq? (car instr) 'save) (eq? (car instr) 'restore))))
				 instructions))))
	(define (assign-sources-filter)
	  (define (iter registers result)
	    (if (null? registers)
	      result
	      (let ((reg-record (car registers)))
		(iter (cdr registers) (cons (cons (car reg-record)
						  (map (lambda (instr) (assign-value-exp instr))
						       (filter (lambda (instr) (and (pair? instr) (eq? (car instr) 'assign) (eq? (car reg-record) (assign-reg-name instr))))
							       instructions))) result)))))
	  (iter register-table '()))
	(list (list 'all-instructions (all-instructions-filter))
	      (list 'goto-registers (register-label-filter))
	      (list 'register-save-restore (register-save-restore-filter))
	      (list 'register-assignment (assign-sources-filter))))

      (define (toggle-tracing value)
	(for-each (lambda (reg-record)
		    (let ((register (cadr reg-record)))
		      (if value
			(register 'enable-tracing)
			(register 'disable-tracing))))
		  register-table)
	(set! tracing-enabled? value))

      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'install-data-path) (lambda (controller-text) (set! data-path (make-data-path controller-text))))
	      ((eq? message 'data-path) data-path)
	      ((eq? message 'register-table) register-table)
	      ((eq? message 'print-instruction-execution-count) (print-instruction-execution-counter))
	      ((eq? message 'reset-instruction-execution-counter) (reset-instruction-execution-counter))
	      ((eq? message 'enable-tracing) (toggle-tracing #t))
	      ((eq? message 'disable-tracing) (toggle-tracing #f))
	      (else (error "Unknown request: MACHINE"
			   message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
		 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
	(let ((next-inst (car text)))
	  (if (symbol? next-inst)
	    (begin
	      (if (not (null? insts))
		(set-instruction-label! (car insts) next-inst))
	      (receive insts
		       (cons (make-label-entry next-inst
					       insts)
			     labels)))
	    (receive (cons (make-instruction next-inst)
			   insts)
		     labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each
      (lambda (inst)
	(set-instruction-execution-proc!
	  inst
	  (make-execution-procedure
	    (instruction-text inst)
	    labels machine pc flag stack ops)))
      insts)))

(define (make-instruction text) (list text '() #f))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cadr inst))
(define (instruction-label inst) (caddr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))
(define (set-instruction-label! inst label)
  (set-car! (cddr inst) label))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label: ASSEMBLE"
	     label-name))))

(define (make-execution-procedure
	  inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else
	  (error "Unknown instruction type: ASSEMBLE"
		 inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
	  (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	    (if (operation-exp? value-exp)
	      (make-operation-exp
		value-exp machine labels operations)
	      (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda () ; execution procedure for assign
	(set-contents! target (value-proc))
	(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc
	      (make-operation-exp
		condition machine labels operations)))
	(lambda ()
	  (set-contents! flag (condition-proc))
	  (advance-pc pc)))
      (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts
	      (lookup-label
		labels
		(label-exp-label dest))))
	(lambda ()
	  (if (get-contents flag)
	    (set-contents! pc insts)
	    (advance-pc pc))))
      (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts (lookup-label
			  labels
			  (label-exp-label dest))))
	     (lambda () (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg (get-register
			machine
			(register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc
	      (make-operation-exp
		action machine labels operations)))
	(lambda () (action-proc) (advance-pc pc)))
      (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts (lookup-label
			labels
			(label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine (register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
			 operations))
	(aprocs
	  (map (lambda (e)
		 (make-primitive-exp e machine labels))
	       (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation: ASSEMBLE"
	     symbol))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? tag (car exp))))

;; ---

(define test
  '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n  1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n) ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n  1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n  2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val) ; save Fib(n  1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n  2)
     (assign n (reg val)) ; n now contains Fib(n  2)
     (restore val) ; val now contains Fib(n  1)
     (restore continue)
     (assign val ; Fib(n  1) + Fib(n  2)
	     (op +) (reg val) (reg n))
     (goto (reg continue)) ; return to caller, answer is in
     val
     immediate-answer
     (assign val (reg n)) ; base case: Fib(n) = n
     (goto (reg continue))
     ))

(define fact-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '((perform (op reset-instruction-execution-counter))
      (assign continue (label fact-done))
      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done
      (perform (op print-instruction-execution-count)))))

(define (fact-loop)
  (println "Factorial Input n:")
  (let ((n (read)))
    (set-register-contents! fact-machine 'n n)
    (fact-machine 'enable-tracing)
    (start fact-machine)
    (println "n!:" (get-register-contents fact-machine 'val))
    ((fact-machine 'stack) 'print-statistics)
    ((fact-machine 'stack) 'initialize)
    (newline)
    (fact-loop)))

(newline)
(fact-loop)
