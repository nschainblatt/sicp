(define (main)
  (let ((matrix-1 (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
        (matrix-2 (list (list 1 2) (list 5 6) (list 8 9) (list 10 12)))
        (vector (list 1 2 3 4)))
    (matrix-*-matrix matrix-1 matrix-2)))

(define (matrix-*-vector m v)
  (map (lambda (row) (accumulate + 0 (accumulate-n * 1 (list row v)))) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m1 m2)
  (let ((cols (transpose m2)))
    (map (lambda (m1-row)
           (map (lambda (m2-row) (accumulate + 0 (accumulate-n * 1 (list m1-row m2-row))))
                cols))
         m1)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (println x)
  (display x)
  (newline))
