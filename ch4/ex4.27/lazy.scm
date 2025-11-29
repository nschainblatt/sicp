(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))
;;; L-Eval input:
count
;;; L-Eval value:
1 ;; Reason: (id 10) was not evaluated as it is not a primitive procedure and is not required to print the value of count.
;;; L-Eval input:
w ;; currently set to (id 10), and when printing like this, will force it's evaluation.
;;; L-Eval value:
10 ;; Reason: printing the value of w forces all expressions to be evaluated involved in it's definition, as the definition of
;;                    the driver-loop indicates.
;;; L-Eval input:
count
;;; L-Eval value:
2 ;; Reason: id was called twice now, incrementing count twice.
