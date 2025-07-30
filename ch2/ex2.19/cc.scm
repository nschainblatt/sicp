;; The order of coins does not matter with this implementation.
;; The reason is because the tree recursion used in procedure cc
;; explores all possible combinations of the coins. Each call to the
;; procedure cc makes two recursive calls with the same coin being operated on.
;; The first recursive call uses a sublist with the first coin removed, while
;; the second one uses the face value of that same first coin to subtract from
;; the amount.

(define (main)
  (display (cc 100 (list 1 5 10 25 50)))
  (newline)
  (display (cc 100 (list 10 5 25 1 50)))
  (newline))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))
