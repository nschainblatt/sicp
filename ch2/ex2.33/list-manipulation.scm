(define (main)
  (let ((list-1 (list 1 2 3 4))
        (list-2 (list 5 6 7 8)))
    (display (my-map square list-1))
    (newline)
    (display (my-append list-1 list-2))
    (newline)
    (display (my-length (my-append list-1 list-2)))
    (newline)))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

;; Append with accumulate follows the regular append algorithm, accumulate 'cdr's down the passed sequence until the end, then returns
;; the initial (seq2), leading to seq2 being appended onto seq1 in the new list returned.
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

;; Regular appends works by 'cdr'ing down seq1 until the end while building up the new list by 'car'ing seq1, then it returns seq2 to be
;; the next cdr value of the new list, leading to seq2 being appended onto seq1.
(define (my-regular-append seq1 seq2)
  (define (recur seq)
    (if (null? seq)
      seq2
      (cons (car seq) (recur (cdr seq)))))
  (recur seq1))

(define (my-length sequence)
  (accumulate (lambda (curr acc) (inc acc)) 0 sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (inc x) (+ x 1))

(define (square x) (* x x))
