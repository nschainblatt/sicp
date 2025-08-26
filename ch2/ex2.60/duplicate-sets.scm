(define (main)
  (let ((set1 '(1 2 2 3 4 4 5))
        (set2 '(1 1 2 2 2 2 3 4 5 6 6 6 7 8 9 10)))
    (println (intersection-set set1 set2))
    (println (union-set set1 set2))))

(define (println x)
  (display x)
  (newline))

;; No change, efficiency is the same
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; We don't check if the element is already in the set, efficiency is now constant instead of linear
(define (adjoin-set x set)
  (cons x set))

(define (custom-adjoin-set bi-predicate)
  (lambda (x set)
    (if (bi-predicate x set)
      set
      (cons x set))))

;; Even if the set now allows duplicates, the union operation will still need to return a set that has no duplicates because of how
;; union is defined. This was updated to remove the use of adjoin-set since the new version of adjoin-set defined above would allow
;; duplicates. The efficiency of this procedure is quadratic.
(define (union-set set1 set2)
  (accumulate (custom-adjoin-set element-of-set?) (accumulate (custom-adjoin-set element-of-set?) '() set2) set1))

;; As with unions, this also returns a set that has no duplicates.
;; The efficiency of this procedure is quadratic.
(define (intersection-set set1 set2)
  (define (bi-predicate x set)
    (or (not (element-of-set? x set1)) (element-of-set? x set)))
  (accumulate (custom-adjoin-set bi-predicate) '() set2))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))
