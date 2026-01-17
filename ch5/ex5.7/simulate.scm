; a. Recursive exponentiation:
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define expt-recursive-machine
  (make-machine
    '(continue n b val)
    (list (list '= =) (list '- -) (list '* *))
    '(assign continue (label expt-done))
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
    expt-done))

(set-register-contents! expt-recursive-machine n 2)
(set-register-contents! expt-recursive-machine b 2)
(start expt-recursive-machine)
(get-register-contents! expt-recursive-machine val)

; b. Iterative exponentiation:
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1)
		 (* b product))))
  (expt-iter n 1))

(define expt-iter-machine
  (make-machine
    '(n counter b product)
    (list (list '= =) (list '- -) (list '* *))
    (assign counter (reg n))
    (assign product (const 1))
    expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt-loop))
    expt-done))

(set-register-contents! expt-iter-machine n 2)
(set-register-contents! expt-iter-machine b 2)
(start expt-iter-machine)
(get-register-contents! expt-iter-machine product)
