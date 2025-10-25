;; Delay needed for n-bit ripple carry adder:
;;
;; Since each bit in the list will be ran against a full-adder, and any actions attached to previous wires will be ran
;; if those wires change value (note that with the adder, only output wires are written too, so this shouldn't happen),
;; the delay sum is like this for one bit, so multiply by n:
;;
;; Individual delays without running any gates in parallel:
;; 2 * half-adder + or-gate => 2 * (or-gate + and-gate + invert,and-gate) + or-gate
;;
;; Note that gates with no input wires are waiting on another gates output wires (no dependencies) can be ran at the same time.
;; Same number means they can be ran at the same time:
;; (or-gate a b d) ;;1
;; (and-gate a b c);;1
;; (inverter c e)  ;;2
;; (and-gate d e s);;3
;; ^ This is for one half-adder, note that there are two in a full-adder, one uses the output of the other so they are dependent
;; and can't be ran at the same time (the second half-adder depends on the sum from the first ones output).
;;
;; Because of these dependencies the total delay would be the sum of two complete half-adders plus an additional or-gate-delay
;; from the full-adder.
;;
;; The first or-gate and and-gate in each half-adder can be ran at the same time because they are independent.
;; So assuming an or-gate and a and-gate have the same delay, we can remove one of the delays from the answer, otherwise
;; you would have to include the difference.
;;
;; Example 1: Assuming or-gate and and-gate have same delay (same delay and they can be ran at the same time, so that
;; delay is equal to one of them, I chose to keep or-gate-delay):
;; 2 * (or-gate-delay + invert + and-gate) + or-gate
;;
;; Example 2: Assuming or-gate and and-gate have a different delay:
;; 2 * (max(or-gate-delay, and-gate-delay) + invert-delay + and-gate-delay) + or-gate-delay
;;
;; Example 3: No optimizations
;; 2 * (or-gate-delay + and-gate-delay + invert-delay + and-gate-delay) + or-gate-delay
;;
;; You would then multiply the total delay (choose one of the examples) by the number of bits to get the total delay for the
;; ripple-carry-adder.
;;
;; After checking my estimation against the total number of gates ran, the actual result is exaclty double my estimate.
;; This is because in each and-gate, and or-gate, we add two actions for the two input wires (to rerun on change).
;; In add-action! we invoke the action being added and add the action to the action list, so we essentially run two and-gates
;; for every and-gate called.

(define (main)
  (let ((An (list (make-wire 0) (make-wire 1) (make-wire 0)))
	(Bn (list (make-wire 1) (make-wire 1) (make-wire 1)))
	(Sn (list (make-wire) (make-wire) (make-wire)))
	(C (make-wire)))

    ;; Create actions for adders
    (ripple-carry-adder An Bn Sn C)

    ;; not changing any values, just setting to same value to trigger adders.
    ; (for-each (lambda (wire) (set-signal! wire (get-signal wire))) An)
    ; (for-each (lambda (wire) (set-signal! wire (get-signal wire))) Bn)

    (display "SUM: ")
    (println (map (lambda (wire) (get-signal wire)) Sn))
    (display "CARRY BIT: ")
    (println (get-signal C))

    (display "TOTAL AND-GATE COUNT:")
    (println total-and-gate-count)
    (display "TOTAL OR-GATE COUNT:")
    (println total-or-gate-count)
    (display "TOTAL INVERTER COUNT:")
    (println total-inverter-count)))

(define (ripple-carry-adder An Bn Sn C)
  (if (not (= (length An) (length Bn)))
    (error "binary numbers do not contain the same number of bits --RIPPLE-CARRY-ADDER")
    (let ((n (length An)))

      (define (last-wire? bit-list)
	(= (length bit-list) (length An)))

      (define (recur sub-An sub-Bn sub-Sn)
	;; note only have to check one list since they are all the same length
	(if (null? sub-An)
	  (make-wire)
	  (let ((a (car sub-An))
		(b (car sub-Bn))
		(c-in (recur (cdr sub-An) (cdr sub-Bn) (cdr sub-Sn)))
		(c-out (make-wire))
		(sum (car sub-Sn)))

	    ;; When on the last wire (technically the first in the input list, but it's last to be added due to the recursive process),
	    ;; pass C as the c-out wire, otherwise pass the local c-out wire as the c-out wire in full-adder.
	    ;; This allows the expected output wire C to obtain the final updated carry value.
	    (full-adder a b c-in sum (if (last-wire? sub-An) C c-out))

	    c-out)))

      (recur An Bn Sn))))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok-full-adder))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d) ;;1
    (and-gate a b c);;1
    (inverter c e)  ;;2
    (and-gate d e s);;3
    'ok-half-adder))

;; --- WIRE ---
(define (get-signal wire)
  (wire 'get-signal))
(define (get-actions wire)
  (wire 'get-actions))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire new-action)
  ((wire 'add-action!) new-action))

(define (make-wire . args) ;; First arg in optional args list will be default value of signal
  (let ((signal (if (and (not (null? args)) (bit? (car args))) (car args) 0))
	(actions '()))
    (define (set-signal! new-value)
      (if (not (bit? new-value))
	(error "Invalid value --SET-SIGNAL!")
	(if (not (= new-value signal))
	  (begin
	    (set! signal new-value)
	    ;; For some reason we must call all actions even if the new-value is the same as the previous value.
	    ;; If we don't, then the sum and carry bits are not correct.
	    (call-actions)
	    'ok-set-signal!))))
    (define (call-actions)
      (if (not (null? actions))
      (println "CALLING ACTIONS"))
      (for-each (lambda (action) (action)) actions))
    ;; Makes sure to add actions in order they were created
    (define (add-action! op)
      (if (null? actions)
	(set! actions (cons op '()))
	(set-cdr! actions (cons op '())))
      (op))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
	    ((eq? m 'set-signal!) set-signal!)
	    ((eq? m 'add-action!) add-action!)
	    ((eq? m 'get-actions) actions)
	    (else (error "Invalid operations --MAKE-WIRE"))))
    dispatch)))


;; --- GATE ---
(define and-gate-delay 1)
(define total-and-gate-count 0)
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (set! total-and-gate-count (inc total-and-gate-count))
    (let ((new-value
	    (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok-and-gate)
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
    1
    0))

(define or-gate-delay 1)
(define total-or-gate-count 0)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (set! total-or-gate-count (inc total-or-gate-count))
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok-or-gate)

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
    1
    0))

(define inverter-delay 1)
(define total-inverter-count 0)
(define (inverter a output)
  (define (inverter-action-procedure)
    (set! total-inverter-count (inc total-inverter-count))
    (let ((new-value (logical-inverter (get-signal a))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! a inverter-action-procedure)
  'ok-inverter)

(define (logical-inverter x)
  (if (= x 0) 1 0))

(define (after-delay time op)
  ;; TODO: add delay before calling op
  (op))

(define (!= x y)
  (not (= x y)))

(define (bit? x)
  (or (= x 0) (= x 1)))

(define (println x)
  (display x) (newline))

(define (inc x)
  (+ x 1))

(main)
