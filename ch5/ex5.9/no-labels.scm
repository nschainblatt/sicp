;; Exercise 5.9: The treatment of machine operations above
;; permits them to operate on labels as well as on constants
;; and the contents of registers. Modify the expression-processing
;; procedures to enforce the condition that operations can be
;; used only with registers and constants.

;; During 'make-operation-exp', each operand has a execution procedure created for it using 'map' and
;; 'make-primitive-exp'. So to achieve what this exercise requests, we just have to remove the support
;; for labels in 'make-primitive-exp'.
;; However, we cannot remove the label support from the 'make-primitive-exp' procedure at it is reused during other aspects
;; of the machine design (assign expressions).
;; So we must modify 'make-operation-exp' itself to not support label operands.

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
			 operations))
	(aprocs
	  (map (lambda (e)
		 ;; Changed:
		 (if (label-exp? e)
		   (error "Label expressions are not allowed as operands to operation expressions.")
		   (make-primitive-exp e machine labels)))
	       (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
