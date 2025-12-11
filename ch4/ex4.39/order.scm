;; Original
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

;; I do not believe the order of the require's matter for getting the same answers.

;; I do believe that the order will affect the speed, here is why:

;; Smith is the last person to use amb, and will be the first location to try another answer when any require fails (until it runs
;; out of answers).
;; Since amb selects an answer using depth first search and always starts with the first option, everyone's first answer will be 1,
;; failing the distinct requirement.
;; Once the distinct requirement is met, the next requirement in the original is cooper, lets say this requirement fails, it will go 
;; to Smith and get a new answer and try everything all over again, until Smith, Miller, and Flether run out of options, then Coopers
;; answer will increase.
;; To fix this issue, we should place the requirements in the reverse order the people that are defined.
;; Starting with Smith, then Miller, then Flether, all the way to Baker.
;; Then, when all individual requirements are met, the ones with multiple people may follow, starting with adjacent, and lastly
;; the distinct requirement.
;; Since the first answers are always tried before later ones, the requirements that are based on lower numbers should go first.
;; This is all based soley on my understanding of how retries work, since we don't have an evaluator to test with.

;; Faster
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (> miller cooper))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= cooper 1)))
    (require (not (= baker 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))
