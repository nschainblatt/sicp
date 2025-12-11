(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5))
				(fletcher (amb 1 2 3 4 5)) (miller (amb 1 2 3 4 5))
				(smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ; (require (not (= (abs (- smith fletcher)) 1))) --changed
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker) (list 'cooper cooper)
	  (list 'fletcher fletcher) (list 'miller miller)
	  (list 'smith smith))))

;; Original
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; New
;; ((baker 1) (cooper 2) (miller 3) (fletcher 4) (smith 5))

;; ((baker 1) (cooper 2) (smith 3) (fletcher 4) (miller 5))

;; ((smith 1) (cooper 2) (baker 3) (fletcher 4) (miller 5))

;; ((smith 1) (fletcher 2) (baker 3) (cooper 4) (miller 5))

;; 5 total

;; We have not implemented amb and automatic search into our evaluator, so these were done by hand.
;; There may be more solutions than this.
