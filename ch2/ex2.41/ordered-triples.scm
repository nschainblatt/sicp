(define (main)
  (ordered-triples-that-sum-to-s 5 11))

(define (ordered-triples-that-sum-to-s n s)
  (filter (lambda (triple) (= (list-sum triple) s))
          (ordered-triples n)))

(define (list-sum triple)
  (accumulate + 0 triple))

;; Returns a sequence of triples whose integers are distinct in a triple and are in rance of 1-n.
;; Note that order matters here. For example: a sequence where n = 5 would include (1 2 3) and (2 1 3),
;; these are considered different triples since order matters.
(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (filter (lambda (kx) (and (not (= kx j)) (not (= kx i)))) (enumerate-interval 1 n))))
                      (filter (lambda (jx) (not (= jx i))) (enumerate-interval 1 n))))
           (enumerate-interval 1 n)))

(define (flatmap op sequence)
  (accumulate append '() (map op sequence)))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval start end)
  (if (> start end)
    '()
    (cons start (enumerate-interval (+ start 1) end))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate sequence))))
