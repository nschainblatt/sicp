;; d.
;; Changing the representation within the constructors to utilize a cons instead of a list procedure makes the underlying
;; structure hold the mobile components in a improper list/pair. Before with the list procedure, we guaranteed the storage of all
;; components in a mobile to be in a proper list.
;; To handle this change, the selectors must be updated to return the correct components that are stored in a slightly different location.
;; Components are now pairs of pairs, instead of lists of lists.
;; This only requires having to remove the extra call to car in the 'right' positioned components in the pair.

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
  (if (number? mobile)
    mobile
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (+ (total-weight (branch-structure left))
         (total-weight (branch-structure right))))))

;; c.
;; Returns true if a mobiles branches are balanced.
(define (balanced? mobile)
  (if (number? mobile)
    #t
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and (= (torque left) (torque right))
           (balanced? (branch-structure left))
           (balanced? (branch-structure right))))))

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))
