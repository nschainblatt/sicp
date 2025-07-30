(define (main)
  (display (square-list-iter-incorrect-1 (list 1 2 3 4 5)))
  (newline)
  (display (square-list-iter-incorrect-2 (list 1 2 3 4 5)))
  (newline)
  (display (square-list-iter-correct (list 1 2 3 4 5)))
  (newline))


;; This is correct because we are building up the list by keeping the internal structure of the results list correct.
;; Meaning each iteration, after appending, the results list is in this structure: (n1, n2, n3 ..., nil).
;; Utilizing append allows the list to remain intact, and to append each consecutive number to the end of the list
;; in the correct order.
(define (square-list-iter-correct items)
  (define (iter sub-items results)
    (if (null? sub-items)
      results 
      (iter (cdr sub-items) (append results (list (square (car sub-items)))))))
  (iter items '()))

;; Louis's first attempt is incorrect because it will return the answer list in the reverse order.
;; This happens because he iteratively creates new pairs with the right element being the current
;; answers so far, which start from the beginning, leading to a reverse order.
(define (square-list-iter-incorrect-1 items)
  (define (iter sub-items results)
    (if (null? sub-items)
      results 
      (iter (cdr sub-items) (cons (square (car sub-items)) results))))
  (iter items '()))

;; Louis's second attempt is also incorrect as it breaks the structure of a correct list, resulting
;; in a broken output. This happens because he is utilizing cons to construct a list incorrectly.
;; Utilizing cons in this way will create sequences like this throughout the process:
;; (() 1)
;; ((() 1) 4)
;; (((() 1) 4) 9)
;; and so on.
;; To correctly add an item to the end of the list, you must use append, or create your own version of append
;; to iterate to the end of the list and the return a new list the desired in place.
(define (square-list-iter-incorrect-2 items)
  (define (iter sub-items results)
    (if (null? sub-items)
      results 
      (iter (cdr sub-items) (cons results (square (car sub-items))))))
  (iter items '()))

(define (square x) (* x x))
