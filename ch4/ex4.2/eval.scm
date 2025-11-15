;; a.
;; Moving the clause for procedure applications up any amount at all will provide unexpected results.
;; This was stated earlier in the chapter:
;;
;; A procedure application is any compound expression that is not
;; one of the above expression types.
;;
;; So the clause for procedure application must be the last non-else clause in the case analysis.
;;
;;
;; b.
(define (application? exp)
  (tagged-list? exp 'call))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))
