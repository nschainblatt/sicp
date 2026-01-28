Exercise 5.28: Modify the definition of the evaluator by
changing eval-sequence as described in Section 5.4.2 so
that the evaluator is no longer tail-recursive. Rerun your
experiments from Exercise 5.26 and Exercise 5.27 to demon-
strate that both versions of the factorial procedure now
require space that grows linearly with their input.

```scm
ev-sequence
    (test (op no-more-exps?) (reg unev))
    (branch (label ev-sequence-end))
    (assign exp (op first-exp) (reg unev))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
ev-sequence-end
    (restore continue)
    (goto (reg continue))
```


|            | Max Depth      | Total Pushes    |
| ---------- | -------------- | --------------- |
| Recursive  | 8n + 3         | 34n - 16        |
| Iterative  | 3n + 14        | 37n + 33        |

Notice how both recursive and iterative versions have a max depth that scales linearly with n.
Both of the max depths and total pushes also increased from their previous versions due to the extra saves.
