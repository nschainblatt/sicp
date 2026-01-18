;; Exercise 5.11:
;;
;; When we introduced save and restore in Section 5.1.4,
;; we didn't specify what would happen if you tried to
;; restore a register that was not the last one saved, as
;; in the sequence:
;;
;; (save y) (save x) (restore y)
;;
;; There are several reasonable possibilities for the meaning
;; of restore:
;;
;; a. (restore y) puts into y the last value saved on the
;; stack, regardless of what register that value came from.
;; This is the way our simulator behaves. Show how to
;; take advantage of this behavior to eliminate one instruction
;; from the Fibonacci machine of Section 5.1.4
;; (Figure 5.12).

(controller
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
  ; (restore continue) -- 1/2 These two instructions (restore and save of continue) are not required with the current behavior.
  ;                    --     The order of the stack is the same regardless of whether these instructions are here or not.
  ;; set up to compute Fib(n  2)
  (assign n (op -) (reg n) (const 2))
  ; (save continue)    -- 2/2
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
  fib-done)



;; b. (restore y) puts into y the last value saved on the
;; stack, but only if that value was saved from y; other-
;; wise, it signals an error. Modify the simulator to be-
;; have this way. You will have to change save to put
;; the register name on the stack along with the value.

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (make-reg-pair reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name)))
    (lambda ()
      (let ((reg-pair (pop stack)))
	(if (eq? (reg-pair-name reg-pair) reg-name)
	  (begin (set-contents! reg (reg-pair-contents reg-pair)) (advance-pc pc))
	  (error "Register name mismatch! --MAKE-RESTORE"))))))

(define (make-reg-pair name contents)
  (cons name contents))
(define (reg-pair-name reg-pair)
  (car reg-pair))
(define (reg-pair-contents reg-pair)
  (cdr reg-pair))

;; c. (restore y) puts into y the last value saved from
;; y regardless of what other registers were saved after
;; y and not restored. Modify the simulator to behave
;; this way. You will have to associate a separate stack
;; with each register. You should make the initialize-
;; stack operation initialize all the register stacks.

;; I will implement the stack-per-register design like
;; the exercise suggested. Another alternative that would work with one
;; stack could be to just search for that register by name
;; in the stack using the pairs from part b, and then returning
;; the first match or signaling an error if no match was found.
;; This isn't nearly as performant as we would have to be searching
;; instead of instantly returning a value, but would require less space.

;; Since we must continue to support the procedure 'allocate-register' in 'make-new-machine',
;; we must store all the stacks in a list to allow dynamic allocation of registers
;; that aren't always know ahead of time. These stacks will be tagged by the register name
;; so we are able to efficiently search for them later on during assembly time.
;; The register stacks are located during assembly time (not in the execution procedures)
;; instead of at simulation time to allow the execution speed of the simulation to remain unchanged,
;; all while allowing the flexibility of saving and restoring registers not in the exact order as 
;; if we had a single stack for all registers.

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

(define (make-execution-procedure
	  inst labels machine pc flag stack-list ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack-list pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack-list pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else
	  (error "Unknown instruction type: ASSEMBLE"
		 inst))))

(define (make-new-machine)
  (let* ((pc (make-register 'pc))
	(flag (make-register 'flag))
	;; Create stacks for default registers
	(stack-list (list (make-reg-stack 'pc) (make-reg-stack 'flag)))
	(the-instruction-sequence '()))
    (let ((the-ops
	    (list (list 'initialize-stack
			;; Initialize every stack in the stack-list
			(lambda () (for-each (lambda (stack)
					       (stack 'initialize)) stack-list)))))
	  (register-table
	    (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	  (error "Multiply defined register: " name)
	  (begin (set! register-table
		   (cons (list name (make-register name))
			 register-table))
		 ;; Create stacks for new registers.
		 (set! stack-list (cons (make-reg-stack name) stack-list))))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	    (cadr val)
	    (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	    'done
	    (begin
	      ((instruction-execution-proc (car insts)))
	      (execute)))))
      ;; Constructor and selectors for helper pair.
      (define (make-reg-stack name)
	(cons name (make-stack)))
      (define (reg-stack-name reg-stack)
	(car reg-stack))
      (define (reg-stack-stack reg-stack)
	(cdr reg-stack))
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
	      ((eq? message 'stack-list) stack-list)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request: MACHINE"
			   message))))
      dispatch)))

;; Updated save and restore operations to support the stack-list:
(define (make-save inst machine stack-list pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name)))
    (lambda ()
      (push stack-list (make-reg-pair reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack-list pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name)))
    (lambda ()
      (let ((reg-pair (pop stack-list)))
	(if (eq? (reg-pair-name reg-pair) reg-name)
	  (begin (set-contents! reg (reg-pair-contents reg-pair)) (advance-pc pc))
	  (error "Register name mismatch! --MAKE-RESTORE"))))))

;; Helpers
(define (make-reg-pair name contents)
  (cons name contents))
(define (reg-pair-name reg-pair)
  (car reg-pair))
(define (reg-pair-contents reg-pair)
  (cdr reg-pair))
