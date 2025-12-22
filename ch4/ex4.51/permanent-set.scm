(cd "../amb-eval")
(load "amb-eval.scm")

;; Copy this section into the amb evaluator
; (define (an-element-of seq)
;   (if (null? seq)
;     (amb)
;     (amb (car seq) (an-element-of (cdr seq)))))

; (define (require p)
;   (if (not p)
;     (amb)))

; (define count 0)
; (let ((x (an-element-of '(a b c)))
;       (y (an-element-of '(a b c))))
;   (permanent-set! count (+ count 1))
;   (require (not (eq? x y)))
;   (list x y count))

;; ---

;; a.

;; Added this to the amb-evaluator in ../amb-eval/analyze-eval.scm
;; in order to get the required output.
; (define (permanent-assignment? exp)
;   (tagged-list? exp 'permanent-set!))
;
; (define (analyze-permanent-assignment exp)
;   (let ((var (assignment-variable exp))
;         (vproc (analyze (assignment-value exp))))
;     (lambda (env succeed fail)
;       (vproc env
;              (lambda (val fail2) ; *1*
;                (set-variable-value! var val env)
;                (succeed 'ok fail2))
;              fail))))

;; b.

;; If we had used set! instead of permanent-set!, we would get 1 for the value of count within the returned
;; list every time. This is because the assignment is undone with every explicit and implicit retry (backtracking).

(define the-global-environment (setup-environment))
(driver-loop)
