(define (yacth)
 (let ((sir-barnacle (make-daughter-yacht 'meliassa 'gabrielle))
       (mr-moore (make-daughter-yacht 'mary-ann 'lorna))
       (mr-hall (make-daughter-yacht (amb 'gabrielle 'lorna) 'rosalind)))
  (let ((colonel-downing (make-daughter-yacht  (amb 'gabrielle 'lorna 'rosalind) 'melissa)))
   ;; Ensure mr-hall and colonel-downings daughters don't equal as they we're determined using amb as they are not known ahead of time.
   (require (not (eq? (daughter mr-hall) (daughter colonel-downing))))
   (let ((dr-parkers-daughter (amb (yacht sir-barnacle) (yacht mr-moore) (yacht mr-hall) (yacht colone-downing))))
    ;; Ensure dr-parkers daughter is not equal to any of the others.
    (require (and (not (eq? dr-parkers-daughter (daughter sir-barnacle))) ;; dr-parkers daughter will be one of the others yachts.
	      (not (eq? dr-parkers-daughter (daughter mr-moore)))
	      (not (eq? dr-parkers-daughter (daughter mr-hall)))
	      (not (eq? dr-parkers-daughter (daughter colonel-downing)))))
    (let ((dr-parkers-yacht (amb (daughter sir-barnacle) (daughter mr-moore) (daughter mr-hall) (daughter colone-downing))))  ;; dr-parkers yacht will be one of the others daughters
     ;; Ensure dr-parkers yacht is not equal to any of the others.
     (require (and (not (eq? (yacht dr-parker) (yacht sir-barnacle)))
	       (not (eq? (yacht dr-parker) (yacht mr-moore)))
	       (not (eq? (yacht dr-parker) (yacht mr-hall)))
	       (not (eq? (yacht dr-parker) (yacht colonel-downing)))))
     (list sir-barnacle mr-moore mr-hall colonel-downing dr-parker))))))

;; Initial state (* is amb usage)
;; sir-barnacle:     daughter=melissa    yacht=gabrielle
;; mr-moor:          daughter=mary-ann   yacht=lorna
;; mr-hall:          daughter=gabrielle* yacht=rosalind 
;; colonel-downing:  daughter=gabrielle* yacht=melissa
;; dr-parker:        daughter=gabrielle* yacht=melissa*

;; After requirements forcing alternate choices for amb
;; sir-barnacle:     daughter=melissa    yacht=gabrielle
;; mr-moor:          daughter=mary-ann   yacht=lorna
;; mr-hall:          daughter=gabrielle* yacht=rosalind 
;; colonel-downing:  daughter=lorna*     yacht=melissa
;; dr-parker:        daughter=rosalind*  yacht=mary-ann*

;; Alternative ending
;; sir-barnacle:     daughter=melissa    yacht=gabrielle
;; mr-moor:          daughter=mary-ann   yacht=lorna
;; mr-hall:          daughter=lorna*     yacht=rosalind 
;; colonel-downing:  daughter=rosalind*  yacht=melissa
;; dr-parker:        daughter=gabrielle* yacht=mary-ann*

;; Lorna's father is: colonel-downing in the initial, but as you can see there are many alternative possibilities.

;; If we remove the fact that we know Mary Anns last name, there would be a lot more solutions, as there would be an additional usage of amb.
;; Since there are already more than one posibilities, decreasing the known facts would only increase the solution count.

(define make-daughter-yacht cons)
(define daughter car)
(define yacht cdr)
