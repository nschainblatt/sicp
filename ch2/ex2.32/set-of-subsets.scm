(define (main)
  (subsets (list 1 2 3)))

;; Returns the set of all subsets of the set 's'.
;; This procedure works by recursively building up the set 'rest' and returning it as the set of all subsets of 's'.
;; The goal is to reach the base case where the initial value of 'rest' is just the empty list nil.
;; 'rest' is built up as the recursive procedure contracts after reaching the base case, new elements are combined with
;; every existing element in the current version of 'rest'. Because the first element in 'rest' is the empty list nil,
;; new elements are added as single item lists (a one item list is a pair of an item with nil) first, and are then combined
;; with the other 'rest' elements. So when a new element is retrieved with 'car' it is first combined with nil and added to
;; the list that 'map' returns, and then is combined with every existing element in 'rest'. When 'map' finishes, the built list
;; is appended to the current version of 'rest' and returned to the next call in the call-stack to be used as the next version of 'rest'.
;; Finally the last value of 'rest' is returned as the set of all subsets of original set 's'.
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (define (combiner sub-rest)
        (append (list (car s)) sub-rest))
      (append rest (map combiner rest)))))
