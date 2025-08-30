(define (main)
  (let ((set-of-records (make-tree (make-record  7 (list 1 2 3 4))
                                   (make-tree (make-record  3 (list 5 6 7 8))
                                              (make-tree (make-record  1 (list 9 10 11 12)) '() '())
                                              (make-tree (make-record  5 (list 13 14 15 16)) '() '()))
                                   (make-tree (make-record  9 (list 17 18 19 20))
                                              '()
                                              (make-tree (make-record  50 (list 21 22 23 24)) '() '())))))
    (println (lookup 5 set-of-records)) ;; (13 14 15 16)
    (println (lookup 8 set-of-records)))) ;; #f


;; set-of-records is a binary tree, keys ordered by numerical value ascending.
;; check if tree is null, if null return false
;; if not null, check the value of the key of the entry, if equal, return the data within the entry
;; otherwise, if greater, go right, otherwise go left.
;; O(logn)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= (key (entry set-of-records)) given-key) (data (entry set-of-records)))
        ((> (key (entry set-of-records)) given-key) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))


(define (make-record key data)
  (list key data))
(define key car)
(define data cadr)


(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
