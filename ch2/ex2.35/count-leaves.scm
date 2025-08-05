(define (main)
  (count-leaves (list (list 1 2) (list 1 2 '() 4))))

;; Returns the total number of leaves in a tree by recursively accumulating each nested trees count of
;; of leaves by means of addition.
(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (t)
                     (cond ((null? t) 0) ;; Depends if we want to consider nil a leaf or not.
                           ((pair? t) (count-leaves t))
                           (else 1)))
                   tree)))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (inc x) (+ x 1))
