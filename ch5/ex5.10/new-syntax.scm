;; To update the register machine language to this new syntax, all that is required
;; is to replace the existing procedures that have the same names as the ones below
;; with these versions. This is a direct replacement because the procedure names are kept
;; the same to ensure that no changes are required in the actual implementation of the simulator.

;; Here I have redefined the tag that goes on labels in our language. Making it clear
;; that going to labels is similar to utilizing procedures.
;; This change would require all controllers to be written in this function syntax instead
;; of using the label symbol.
(define (label-exp? exp) (tagged-list? exp 'function))

;; Note that many more changes to syntax procedures could be made, not just to the names
;; of the expressions but also to how they are represented internally (such as a different list structure).
