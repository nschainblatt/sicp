# Exercise 5.31:

In evaluating a procedure application, the
explicit-control evaluator always saves and restores the env
register around the evaluation of the operator, saves and
restores env around the evaluation of each operand (except
the final one), saves and restores argl around the evalua-
tion of each operand, and saves and restores proc around
the evaluation of the operand sequence. For each of the fol-
lowing combinations, say which of these save and restore
operations are superfluous and thus could be eliminated by
the compiler’s preserving mechanism:

Any save I mention requires a restore after returning from a sub-expression, so I don't mention the restores for each save.

## (f 'x 'y)
Avoid: env, argl, proc

Reason:
We could remove all save and restore operations for all registers of this simple combination.
This is because the operator is just a variable we have to lookup with lookup-variable-value, which doesn't modify any
registers that are required for the rest of this combinations evaluation.
All operands are just quoted expressions which we use text-of-quotation to obtain their values.
These operations don't modify any required registers and thus we don't have to save any registers.

## ((f) 'x 'y)
Avoid: env, argl, proc

Reason:
We don't have to save the 'env' register because the operands don't require the 'env' to be evaluated since they are quoted.
Obtaining the value of quoted expressions doesn't modify any of our required registers.
The rest of the registers don't have to be saved because of the quoted operands not requiring them.

## (f (g 'x) y)
Avoid: None, all registers are required to be saved in this combination.

Reason:
f and y are variables to lookup, but the first operand of the combination is another application.
We don't require any saves for obtaining the operator, however for the
first argument we must save the 'env' and 'proc' before applying to later use
to evaluate the third operand y with the lookup-variable-value operation and to apply the original combination with 'proc'.
We must also save the argl register as the application would overwrite this (even if it's argument is just a quoted expression).
Lastly, when evaluating y, we don't require saving any registers because we are just looking up a variable in the current environment and adding it to 'argl'.

## (f (g 'x) 'y)
Avoid: 'env'

Reason:
f is a variable we must lookup in order to get the proc, then we must save 'proc'
before applying the second arguments procedure. We must also save 'argl' to ensure we get the right list back.
Lastly, for 'y it is a quote so we just obtain the quoted contents without saving any registers.
We don't have to save the 'env' register because the operator from f doesn't modify the 'env', and we can extend the environment for the second operand
application without saving 'env' because we don't require it to get the quoted contents of 'y.
