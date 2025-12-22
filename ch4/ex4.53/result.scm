(cd "../amb-eval")
(load "amb-eval.scm")

; (define (require p)
;   (if (not p)
;     (amb)))
;
; (define (an-element-of seq)
;   (if (null? seq)
;     (amb)
;     (amb (car seq) (an-element-of (cdr seq)))))
;
; (define (prime-sum-pair seq1 seq2)
;   (let ((a (an-element-of seq1))
; 	(b (an-element-of seq2)))
;     (require (prime? (+ a b)))
;     (list a b)))
;
; (let ((pairs '()))
;   (if-fail
;     (let ((p (prime-sum-pair '(1 3 5 8)
; 			     '(20 35 110))))
;       (permanent-set! pairs (cons p pairs))
;       (amb))
;     pairs))

;; The result of evaluating this expression which uses both 'if-fail' and 'permanent-set!' special forms is:

;; ((8 35) (3 110) (3 23))

;; Reason:
;; Procedure prime-sum-pair finds the pair (assuming amb choice is sequential, not random) which is (3 23).
;; We update pairs forcefully with permanent-set! to ensure pairs doesn't get reset during backtracking.
;; Then we force failure with (amb) to backtrack into the prime-sum-pair amb usages to generate the rest of the possible pairs.
;;
;; Once we have exhausted all options, the final fail happens inside prime-sum-pair, signalling no more options.
;; The list is then printed to the screen, try-again will not work as there are no more options.

(define the-global-environment (setup-environment))
(driver-loop)
