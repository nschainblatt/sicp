;; Define rules to implement the reverse operation
;; of Exercise 2.18, which returns a list containing the
;; same elements as a given list in reverse order.
;; (Hint: Use append-to-form.) Can your rules answer both
;; (reverse (1 2 3) ?x) and (reverse ?x (1 2 3)) ?

(rule (reverse ?x ?y)
      (?lastY next-to () ?y) ;; Grab the last y element
      (append-to-form (?lastY) ?restX ?x) ;; To satisfy this reverse constraint.
      (append-to-form (?firstY) ?restY ?y) ;; Grab the rest of the y elements.
      (reverse ?restX ?restY)) ;; To recursively assert the constraint
(rule (reverse () ())) ;; until the base case is reached.


;; I believe this works because I am satisfying the constraints of reverse behavior.
;; I am not telling the language how to reverse a list, I am telling it the rules
;; that are in place when a reverse form exists.
;; The implementation of the language will then fill the details of 'how' the
;; correct values are determined to meet the requirements of the rule.
