(cd "../amb-eval")
(load "amb-eval.scm")

;;; Amb-Eval input:
; (define (require p)
;   (if (not p)
;     (amb)))

; (define (an-element-of seq)
;   (if (null? seq)
;     (amb)
;     (amb (car seq) (an-element-of (cdr seq)))))

; (if-fail (let ((x (an-element-of '(1 3 5))))
; (require (even? x))
; x)
; 'all-odd)

;;; Starting a new problem
;;; Amb-Eval value:
; all-odd
;;; Amb-Eval input:
; (if-fail (let ((x (an-element-of '(1 3 5 8))))
; (require (even? x))
; x)
; 'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
;8


;; This was placed in the amb evaluator along with predicate and selectors.
; (define (analyze-if-fail exp)
;   (let ((first (analyze (if-fail-success exp)))
;         (second (analyze (if-fail-failure exp))))
;     (lambda (env succeed fail)
;       (first env
;              (lambda (first-val fail2)
;                (succeed first-val fail2))
;              (lambda () 
;                (second env
;                        (lambda (second-val fail3)
;                          (succeed second-val fail3))
;                        fail))))))

(define the-global-environment (setup-environment))
(driver-loop)
