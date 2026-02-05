;; Example expression that uses preserving:

(compile '(begin 1 (+ 3 4)) 'val 'next)

;; Preserving may save the 'env' register before evaluating (g x), and restore it after.

;; Original preserving output:
(assign val (const 1))
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 4))
(assign argl (op list) (reg val))
(assign val (const 3))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1
;; Notice how the 'env' and 'continue' registers aren't saved due to preserving detecting that they aren't modified by the first instruction (1).

;; ---

;; Modified preserving output to always save and restore listed registers:
(save continue)
(save env)
(save continue)
(assign val (const 1))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save env)
(save continue)
(assign proc (op lookup-variable-value) (const +) (reg env))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save proc)
(save env)
(save continue)
(assign val (const 4))
(restore continue)
(assign argl (op list) (reg val))
(restore env)
(save argl)
(save continue)
(assign val (const 3))
(restore continue)
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(save continue)
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)
after-call1

;; Notice the high number of saves and restores. This is because we always save and restore every register in the 'to preserve' list passed to preserving
;; regardless of whether the first instruction modifies it or the second instruction needs it.
