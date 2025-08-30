(define (main)
  (let ((set1-tree (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 50 '() '()))))
        (set2-tree (make-tree 30
                         (make-tree 1 '() '())
                         (make-tree 70
                                    (make-tree 50 '() '())
                                    (make-tree 90
                                               '()
                                               (make-tree 111 '() '()))))))
  ;; (println set1-tree)
  ;; (println set2-tree)
  ;; (println (union-set set1-tree set2-tree))

  (println set1-tree)
  (println set2-tree)
  (println (tree->list (intersection-set set1-tree set2-tree)))

  ))


(define (println x)
  (display x)
  (newline))


;; Linear O(4*n) -> O(n)
(define (union-set set1-tree set2-tree)
  (define (union set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((> (car set1) (car set2)) (cons (car set2) (union set1 (cdr set2))))
          ((< (car set1) (car set2)) (cons (car set1) (union (cdr set1) set2)))
          ;; If equal, add one of them to the end of the new set
          (else (cons (car set1) (union (cdr set1) (cdr set2))))))
  (list->tree (union (tree->list set1-tree) (tree->list set2-tree))))


;; Linear O(4*n) -> O(n)
(define (intersection-set set1-tree set2-tree)
  (define (intersection set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((> (car set1) (car set2) ) (intersection set1 (cdr set2)))
          ((< (car set1) (car set2) ) (intersection (cdr set1) set2))
          (else (cons (car set1) (intersection (cdr set1) (cdr set2))))))
  (list->tree (intersection (tree->list set1-tree) (tree->list set2-tree))))


(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))
(define entry car)
(define left-branch cadr)
(define right-branch caddr)


;; Linear
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))


;; Linear
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))
