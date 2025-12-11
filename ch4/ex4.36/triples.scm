(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between a b)
  (require (>= b a))
  (amb a (an-integer-between (+ a 1) b)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

;; a. Explain why simply replacing an-integer-between by an-integer-starting-from in the procedure in Exercise 4.35
;;    is not an adequate way to generate arbitrary Pythagorean triples.
;;
;; Key differences: an-integer-starting-from is infinite, an-integer-between is not.
;;
;; My understanding is that since the assignment to k is the last call to an-integer-starting-from (which uses amb), it is the
;; location we jump back to when require fails. Since an-integer-starting-from is infinite, we would just repeatedly try larger values
;; of k, leading to infinite failures since we would still have the original smaller values of i and j.
;; The procedure that creates the value for k, and j, must not be infinite in order to not run forever and to
;; find all possible triples. In other words, we must only have one procedure that may run forever, that is the uppermost procedure
;; that will generate all possible integers to work with.

;; Note that since an-integer-between is not infinite (it has a limit of high in the previous exercise), it will not run forever.
;; This is because once the amb used within assigning k goes through all possible values (j - high), it will go to the amb assigned to
;; j, and so on.

;; If you look at the next exercise in the book, Ben Bitdiddle found a method of finding the value of k, without using amb.
;; We just need a way to do a similar thing for j.
;; The reason that variable 'i' may continue to use an-integer-starting-from even though it is infinite is because it is the
;; uppermost point in the job of finding the triples.
;; This is the only location we may use amb because all other variables must be based around this arbitrary value.
;; We also want to explore all possible integers in order to find all possible triples.


;; b. Write a procedure that actually will accomplish this. (That is, write a procedure for which repeatedly typing try-again would
;; in principle eventually generate all Pythagorean triples.)

;; There is a equation available on Wikipedia that has a method for finding j without amb, soley based on i: https://en.wikipedia.org/wiki/Pythagorean_triple#The_Platonic_sequence
;; Note that I found help on this exercise by reading this: https://wizardbook.wordpress.com/2011/01/12/exercise-4-36-2/

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-starting-from low))
	 (j-limit (if (odd? i)
		    (/ (- (square i) 1) 2)
		    (- (square (/ i 2)) 1)))
    (let ((j (an-integer-between i j-limit)))
      (let ((k (sqrt (+ (square i) (square j)))))
	; (require (= (+ (* i i) (* j j)) (* k k))) ;; I believe this predicate would still work, but the below is simpler.
	(require (integer? k)) ;; This predicate works because it k is an integer, then it satisfies the equation i^2 + j^2 = k^2 because i and j are integers, so k must be an integer for this to be true.
	(list i j k)))))
