;; d.
;; Changing the representation within the constructors to utilize a cons instead of a list procedure makes the underlying
;; structure hold the mobile components in a improper list/pair. Before with the list procedure, we guaranteed the storage of all
;; components in a mobile to be in a proper list.
;; To handle this change, the selectors must be updated to return the correct components that are stored in a slightly different location.
;; Components are now pairs of pairs, instead of lists of lists.
;; This only requires having to remove the extra call to car in the 'right' positioned components in the pair.
;; My helper procedure 'mobile?' will have to change from a 'list?' to 'pair?' primitive procedure to check if the underlying structure is
;; a mobile or not.

(define (main)
  (let ((mobile-1 (make-mobile (make-branch 2 1) (make-branch 2 1))))
    (let ((balanced-mobile-1 (make-mobile (make-branch 2 mobile-1)
                                          (make-branch 2 mobile-1)))
          (balanced-mobile-2 (make-mobile (make-branch 2 (make-mobile (make-branch 2 mobile-1)
                                                                      (make-branch 2 mobile-1)))
                                          (make-branch 2 (make-mobile (make-branch 2 mobile-1)
                                                                      (make-branch 2 mobile-1)))))
          (unbalanced-mobile-1 (make-mobile (make-branch 2 (make-mobile (make-branch 2 mobile-1)
                                                                        (make-branch 2 mobile-1)))
                                            (make-branch 2 (make-mobile (make-branch 2 mobile-1)
                                                                        (make-branch 1 mobile-1))))))

      (display (balanced? balanced-mobile-1))
      (newline)
      (display (balanced? balanced-mobile-2))
      (newline)
      (display (balanced? unbalanced-mobile-1))
      (newline))))

;; Note that it is assumed that every mobile has two branches that consist of either a weight or another mobile in it's structure.
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;; b.
;; Returns the total weight of every branch in a mobile.
(define (total-weight mobile)

  ;; Recursively traverse a mobile, accumulating all of it's weights (leaves of the tree).
  (define (recur sub-mobile)
    (let ((left (left-branch sub-mobile))
          (right (right-branch sub-mobile)))

      ;; The left or the right branch may have as it's structure another mobile
      (+ (if (mobile? (branch-structure left)) (total-weight (branch-structure left)) (branch-structure left))
         (if (mobile? (branch-structure right)) (total-weight (branch-structure right)) (branch-structure right)))))

  (recur mobile))

(define (mobile? m)
  (pair? m))


;; c.
;; Returns true if a mobiles branches are balanced.
(define (balanced? mobile)

  (let ((left (left-branch mobile))
        (right (right-branch mobile)))

    (let ((total-left-weight (if (mobile? (branch-structure left)) (total-weight (branch-structure left)) (branch-structure left)))
          (total-right-weight (if (mobile? (branch-structure right)) (total-weight (branch-structure right)) (branch-structure right)))
          (curr-left-length (branch-length left))
          (curr-right-length (branch-length right)))

      (if (= (* total-left-weight curr-left-length) (* total-right-weight curr-right-length))
        (and (if (mobile? (branch-structure (left-branch mobile))) (balanced? (branch-structure (left-branch mobile))) #t) 
             (if (mobile? (branch-structure (right-branch mobile))) (balanced? (branch-structure (right-branch mobile))) #t))
        #f))))
