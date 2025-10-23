;; The delay time of this new version of or-gate is: 2*inverter-delay + and-gate-delay.
;; The reason the inverter-delay count is 2 and not 3 is because the inverts happening on a1 and a2 can happen at the same time.

(define (main)
  (let ((a1 (make-wire))
	(a2 (make-wire))
	(output (make-wire)))
    (or-gate a1 a2 output)
    (set-signal! a1 0)
    (set-signal! a2 1)
    (println (get-signal output))))

;; --- WIRE ---
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire new-action)
  ((wire 'add-action!) new-action))

(define (make-wire)
  (let ((signal 0)
	(actions '()))
    (define (set-signal! new-value)
      (if (and (!= new-value 0) (!= new-value 1))
	(error "Invalid value --SET-SIGNAL!")
	(if (= new-value signal)
	  'ok
	  (begin
	    (set! signal new-value)
	    (call-actions)
	    'ok))))
    (define (call-actions)
      (define (iter sub-actions)
	(if (null? sub-actions)
	  'ok
	  (begin
	    ((car sub-actions))
	    (iter (cdr sub-actions)))))
      (iter actions))
    (define (add-action! op)
      (set! actions (cons op actions)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
	    ((eq? m 'set-signal!) set-signal!)
	    ((eq? m 'add-action!) add-action!)
	    (else (error "Invalid operations --MAKE-WIRE"))))
    dispatch)))


;; --- GATE ---
(define and-gate-delay 1)
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	    (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
    1
    0))

(define or-gate-delay 1)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((~a1 (make-wire))
	  (~a2 (make-wire))
	  (&~a1~a2 (make-wire)))
      (inverter a1 ~a1)
      (inverter a2 ~a2)
      (and-gate ~a1 ~a2 &~a1~a2)
      (inverter &~a1~a2 output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
    1
    0))

(define inverter-delay 1)
(define (inverter a output)
  (define (inverter-action-procedure)
    (let ((new-value (logical-inverter (get-signal a))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! a (inverter-action-procedure)))

(define (logical-inverter x)
  (if (= x 0) 1 0))

(define (after-delay time op)
  ;; TODO: add delay before calling op
  (op))

(define (!= x y)
  (not (= x y)))

(define (println x)
  (display x) (newline))

(main)
