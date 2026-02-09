# Exercise 5.45

# a.
## Interpreted Factorial - ECEVAL

|            | Max Depth      | Total Pushes    |
| ---------- | -------------- | --------------- |
| Recursive  | 5n + 3         | 32n - 16        |

## Compiled Factorial - ECEVAL-COMPILER

|            | Max Depth            | Total Pushes    |
| ---------- | -------------------- | --------------- |
| Recursive  | 3n - 1  when n > 1   | 6n + 1          |

## Special Purpose Recursive Factorial Machine
Both max depth and total pushes: 2n - 2

## Ratio as constants due to large values of n:
### Ratio between interpreted factorial and compiled
Total pushes: ~5
Max depth: ~1.67

### Ratio between interpreted factorial and special purpose
Total pushes: ~16
Max Depth: ~2.5

The larger constant for the ratio between interpreted and special purpose show that
the special purpose is significantly more efficient in terms of stack operations
than the ratio between interpreted and compiled.

The max depth isn't incredibly more efficient in the special purpose machine than the
compiled or interpreted versions because all of these examples are recursive, so the max
depth is closely related to n.

# b.
```scm
(compile '(define (factorial n)
            (if (= n 1)
                1
                (* (factorial (- n 1)) n)))
         'val
         'next)
```

# Current output of our compiler:

(assign val (op make-compiled-procedure) (label entry2) (reg env)) (goto (label after-lambda1)) entry2 (assign env (op compiled-procedure-env) (reg proc)) (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) (save continue) (save env) (assign proc (op lookup-variable-value) (const =) (reg env)) (assign val (const 1)) (assign argl (op list) (reg val)) (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op cons) (reg val) (reg argl)) (test (op primitive-procedure?) (reg proc)) (branch (label primitive-branch17)) compiled-branch16 (assign continue (label after-call15)) (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) primitive-branch17 (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) after-call15 (restore env) (restore continue) (test (op false?) (reg val)) (branch (label false-branch4)) true-branch5 (assign val (const 1)) (goto (reg continue)) false-branch4 (assign proc (op lookup-variable-value) (const *) (reg env)) (save continue) (save proc) (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op list) (reg val)) (save argl) (assign proc (op lookup-variable-value) (const factorial) (reg env)) (save proc) (assign proc (op lookup-variable-value) (const -) (reg env)) (assign val (const 1)) (assign argl (op list) (reg val)) (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op cons) (reg val) (reg argl)) (test (op primitive-procedure?) (reg proc)) (branch (label primitive-branch8)) compiled-branch7 (assign continue (label after-call6)) (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) primitive-branch8 (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) after-call6 (assign argl (op list) (reg val)) (restore proc) (test (op primitive-procedure?) (reg proc)) (branch (label primitive-branch11)) compiled-branch10 (assign continue (label after-call9)) (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) primitive-branch11 (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) after-call9 (restore argl) (assign argl (op cons) (reg val) (reg argl)) (restore proc) (restore continue) (test (op primitive-procedure?) (reg proc)) (branch (label primitive-branch14)) compiled-branch13 (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) primitive-branch14 (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) (goto (reg continue)) after-call12 after-if3 after-lambda1 (perform (op define-variable!) (const factorial) (reg val) (reg env)) (assign val (const ok))

# Special purpose machine

```scm
(define fact-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '((assign continue (label fact-done))
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
      fact-done)))
```

### Potential Improvements

Since our compiler is general purpose, many steps are taken that the special purpose machine doesn't
have to. Such as defining procedures, managing an environment, and calling procedures.
The only way I see us creating code similar to the machine with our compiler would be to implement
the fact-machine as an open-code primitive, that is create special code generators for factorial
and install them in our compile case matcher.

We could also use our open coded primitive operations from previous exercises.
