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
;;
;; c. (restore y) puts into y the last value saved from
;; y regardless of what other registers were saved aer
;; y and not restored. Modify the simulator to behave
;; this way. You will have to associate a separate stack
;; with each register. You should make the initialize-
;; stack operation initialize all the register stacks.
