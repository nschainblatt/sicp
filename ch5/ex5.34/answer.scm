;; Initial:
;; The iterative code output is decently longer due to the larger number of scheme expressions to compile (two procedures instead of one).
;; 

;; Iterative
(compile '(define (factorial n)
	    (define (iter product counter)
	      (if (> counter n)
		product
		(iter (* counter product)
		      (+ counter 1))))
	    (iter 1 1))
	 'val
	 'next)

((env) (val) ((assign val (op make-compiled-procedure) (label entry2) (reg env))
	      (goto (label after-lambda1))
	      entry2
	      (assign env (op compiled-procedure-env) (reg proc))
	      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
	      (assign val (op make-compiled-procedure) (label entry7) (reg env))
	      (goto (label after-lambda6))
	      entry7
	      (assign env (op compiled-procedure-env) (reg proc))
	      (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
	      (save continue)
	      (save env)
	      (assign proc (op lookup-variable-value) (const >) (reg env))
	      (assign val (op lookup-variable-value) (const n) (reg env))
	      (assign argl (op list) (reg val))
	      (assign val (op lookup-variable-value) (const counter) (reg env))
	      (assign argl (op cons) (reg val) (reg argl))
	      (test (op primitive-procedure?) (reg proc))
	      (branch (label primitive-branch22))
	      compiled-branch21
	      (assign continue (label after-call20))
	      (assign val (op compiled-procedure-entry) (reg proc))
	      (goto (reg val))
	      primitive-branch22
	      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	      after-call20
	      (restore env)
	      (restore continue)
	      (test (op false?) (reg val))
	      (branch (label false-branch9))
	      true-branch10
	      (assign val (op lookup-variable-value) (const product) (reg env))
	      (goto (reg continue))
	      false-branch9
	      (assign proc (op lookup-variable-value) (const iter) (reg env))
	      (save continue)
	      (save proc)
	      (save env)
	      (assign proc (op lookup-variable-value) (const +) (reg env))
	      (assign val (const 1))
	      (assign argl (op list) (reg val))
	      (assign val (op lookup-variable-value) (const counter) (reg env))
	      (assign argl (op cons) (reg val) (reg argl))
	      (test (op primitive-procedure?) (reg proc))
	      (branch (label primitive-branch16))
	      compiled-branch15
	      (assign continue (label after-call14))
	      (assign val (op compiled-procedure-entry) (reg proc))
	      (goto (reg val))
	      primitive-branch16
	      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	      after-call14
	      (assign argl (op list) (reg val))
	      (restore env)
	      (save argl)
	      (assign proc (op lookup-variable-value) (const *) (reg env))
	      (assign val (op lookup-variable-value) (const product) (reg env))
	      (assign argl (op list) (reg val))
	      (assign val (op lookup-variable-value) (const counter) (reg env))
	      (assign argl (op cons) (reg val) (reg argl))
	      (test (op primitive-procedure?) (reg proc))
	      (branch (label primitive-branch13))
	      compiled-branch12
	      (assign continue (label after-call11))
	      (assign val (op compiled-procedure-entry) (reg proc))
	      (goto (reg val))
	      primitive-branch13
	      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	      after-call11
	      (restore argl)
	      (assign argl (op cons) (reg val) (reg argl))
	      (restore proc)
	      (restore continue)
	      (test (op primitive-procedure?) (reg proc))
	      (branch (label primitive-branch19))
	      compiled-branch18 (assign val (op compiled-procedure-entry) (reg proc))
	      (goto (reg val))
	      primitive-branch19
	      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	      (goto (reg continue))
	      after-call17
	      after-if8
	      after-lambda6
	      (perform (op define-variable!) (const iter) (reg val) (reg env))
	      (assign val (const ok))
	      (assign proc (op lookup-variable-value) (const iter) (reg env))
	      (assign val (const 1))
	      (assign argl (op list) (reg val))
	      (assign val (const 1))
	      (assign argl (op cons) (reg val) (reg argl))
	      (test (op primitive-procedure?) (reg proc))
	      (branch (label primitive-branch5))
	      compiled-branch4
	      (assign val (op compiled-procedure-entry) (reg proc))
	      (goto (reg val))
	      primitive-branch5
	      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	      (goto (reg continue))
	      after-call3
	      after-lambda1
	      (perform (op define-variable!) (const factorial) (reg val) (reg env))
	      (assign val (const ok))))

;; Recursive
(compile
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1)) n)))
  'val
  'next)

((env) (val)
       ((assign val (op make-compiled-procedure) (label entry2) (reg env))
	(goto (label after-lambda1))
	entry2
	(assign env (op compiled-procedure-env) (reg proc))
	(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
	(save continue)
	(save env)
	(assign proc (op lookup-variable-value) (const =) (reg env))
	(assign val (const 1))
	(assign argl (op list) (reg val))
	(assign val (op lookup-variable-value) (const n) (reg env))
	(assign argl (op cons) (reg val) (reg argl))
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-branch17))
	compiled-branch16
	(assign continue (label after-call15))
	(assign val (op compiled-procedure-entry) (reg proc))
	(goto (reg val))
	primitive-branch17
	(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	after-call15
	(restore env)
	(restore continue)
	(test (op false?) (reg val))
	(branch (label false-branch4))
	true-branch5 (assign val (const 1))
	(goto (reg continue))
	false-branch4
	(assign proc (op lookup-variable-value) (const *) (reg env))
	(save continue)
	(save proc)
	(assign val (op lookup-variable-value) (const n) (reg env))
	(assign argl (op list) (reg val))
	(save argl)
	(assign proc (op lookup-variable-value) (const factorial) (reg env))
	(save proc)
	(assign proc (op lookup-variable-value) (const -) (reg env))
	(assign val (const 1))
	(assign argl (op list) (reg val))
	(assign val (op lookup-variable-value) (const n) (reg env))
	(assign argl (op cons) (reg val) (reg argl))
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-branch8))
	compiled-branch7
	(assign continue (label after-call6))
	(assign val (op compiled-procedure-entry) (reg proc))
	(goto (reg val))
	primitive-branch8
	(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	after-call6
	(assign argl (op list) (reg val))
	(restore proc)
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-branch11))
	compiled-branch10
	(assign continue (label after-call9))
	(assign val (op compiled-procedure-entry) (reg proc))
	(goto (reg val))
	primitive-branch11
	(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	after-call9
	(restore argl)
	(assign argl (op cons) (reg val) (reg argl))
	(restore proc)
	(restore continue)
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-branch14))
	compiled-branch13
	(assign val (op compiled-procedure-entry) (reg proc))
	(goto (reg val))
	primitive-branch14
	(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	(goto (reg continue))
	after-call12
	after-if3
	after-lambda1
	(perform (op define-variable!) (const factorial) (reg val) (reg env))
	(assign val (const ok))))

