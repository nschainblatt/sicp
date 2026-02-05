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

;; Modified preserving output to always save and restore listed registers:
