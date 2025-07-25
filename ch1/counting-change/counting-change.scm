(define (main)
  (count-change 10))

(define (count-change amount)
  (cc amount 2))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+
                (cc amount (- kinds-of-coins 1))
                (cc (- amount (denomination kinds-of-coins)) kinds-of-coins)
                ))))

(define (denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

