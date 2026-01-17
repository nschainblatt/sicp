;; Exercise 5.8: The following register-machine code is ambiguous,
;; because the label here is defined more than once:
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there

;; a.
;; With the simulator as written, what will the contents of
;; register a be when control reaches there?

;; Solution:
;;    Looking at extract-labels, since the instructions are recursively traversed, the first occurrence of the label 'here'
;;    in the controller instructions will take precedence. Meaning it will be inserted last into the label table. Where it
;;    is placed at the beginning, ahead of the 'here' label at the end of the instructions that was processed first.
;;    Then later, when the instructions for a label are fetched with 'lookup-label' the first occurrence of 'here' is returned
;;    due to the assoc usage returning the first match found.
;;    This all means the contents of register 'a' will be 3 when 'there' is reached.


;; b.
;; Modify the extract-labels procedure so that the assembler will signal an
;; error if the same label name is used to indicate two different
;; locations

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
	(let ((next-inst (car text)))
	  (if (symbol? next-inst)
	    (if (lookup-label next-inst)
	      (error "Duplicate label found!" next-inst)
	      (receive insts
		       (cons (make-label-entry next-inst
					       insts)
			     labels)))
	    (receive (cons (make-instruction next-inst)
			   insts)
		     labels)))))))
