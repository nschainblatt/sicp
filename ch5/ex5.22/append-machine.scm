;; Exercise 5.22: Exercise 3.12 of Section 3.3.1 presented an
;; append procedure that appends two lists to form a new list
;; and an append! procedure that splices two lists together.
;; Design a register machine to implement each of these pro-
;; cedures. Assume that the list-structure memory operations
;; are available as primitive operations.

(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
    (list (list 'cons cons) (list 'car car) (list 'cdr cdr) (list 'null? null?))
    '(
        (assign continue (label append-done))
      append-loop
        (test (op null?) (reg x))
        (branch (label base-case))
        (save x)
        (save continue)
        (assign x (op cdr) (reg x))
        (assign continue (label append-car))
        (goto (label append-loop))
      append-car
        (restore continue)
        (restore x)
        (assign tmp (op car) (reg x))
        (assign val (op cons) (reg tmp) (reg val))
        (goto (reg continue))
      base-case
        (assign val (reg y))
        (goto (reg continue))
      append-done
      ;; answer is in register val
      )))





(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
