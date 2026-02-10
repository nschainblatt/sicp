# Exercise 5.46:

```scm
(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
```

When n >= 2

## Interpreted Fib
|            | Max Depth      | Total Pushes          |
| ---------- | -------------- | --------------------- |
| Recursive  | 5n + 3         | S(n-1) + S(n-2) + 40  |


## Compiled Fib
|            | Max Depth      | Total Pushes        |
| ---------- | -------------- | ------------------- |
| Recursive  | 3n - 1         | S(n-1) + S(n-2) + 3 |

S(2) = 17
S(3) = 27
S(4) = 47

│;;; EC-Eval input:
│(fib 10)
│
│(total-pushes = 887 maximum-depth = 29)
│;;; EC-Eval value:
│55


## Special-Purpose Fib Machine
|            | Max Depth      | Total Pushes        |
| ---------- | -------------- | ------------------- |
| Recursive  | 2n - 2         | S(n-1) + S(n-2) + 4 |

S(2) = 4
S(3) = 8
S(4) = 16

│Fib Input n: 10
│
│fib(n): 55
│(total-pushes = 352 maximum-depth = 18 current-depth = 0)

## Conclusion

The special purpose machine is yet again significantly more efficient with large values of n.
We perform many less stack operations with the machine because we we're able to hand craft
more efficient code than what our general purpose compiler can do.
