(define (main)
  (prime-sum-pairs 6))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

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

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (= (smallest-divisor n) 1))

(define (smallest-divisor n)
  (let ((n-root (sqrt n)))
    (define (smallest-divisor-impl divisor)
      (cond ((> divisor n-root) 1)
            ((= (remainder n divisor) 0) divisor)
            (else (smallest-divisor-impl (+ divisor 1)))))
    (smallest-divisor-impl 2)))
