; Exercise 5.4: Specify register machines that implement each
; of the following procedures. For each machine, write a con-
; troller instruction sequence and draw a diagram showing
; the data paths.


; a. Recursive exponentiation:
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; NOTE:
;; Registers b and n don't need to be saved to the stack because:
;; - Register b never changes
;; - Register n is not used in any delayed operations
;; However, since we are implementing recursion in our controllers,i
;; we are providing a general implementation to work regardless of the problem.
;; This is why we save all registers.
;; In a more sophisticated machine we could detect registers we
;; aren't using or changing later, and could leave them
;; out of the stack.

(controller
    (assign continue (label expt-done))
  expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (save n)
    (save b)
    (assign continue (label after-expt))
    (assign n (op -) (reg n) (const 1))
    (goto (loop expt-loop))
  after-expt
    (restore b)
    (restore n)
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  expt-done)


; b. Iterative exponentiation:
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1)
		 (* b product))))
  (expt-iter n 1))

(controller
    (assign counter (reg n))
    (assign product (const 1))
  expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt-loop))
  expt-done)
