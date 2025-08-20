#lang sicp

(define (main)
  (println (list 'a 'b 'c)) ;; (a b c)
  (println (list (list 'george))) ;; ((george))
  (println (cdr '((x1 x2) (y1 y2)))) ;; ((y1 y2))
  (println (cadr '((x1 x2) (y1 y2)))) ;; (y1 y2)
  (println (pair? (car '(a short list)))) ;; false
  (println (memq 'red '((red shoes) (blue socks)))) ;; false
  (println (memq 'red '(red shoes blue socks)))) ;; (red shoes blue socks)

(define (println . args)
  (define (printer sub-args)
    (cond ((null? sub-args) (newline))
          (else (display (car sub-args))
                (display " ")
                (printer (cdr sub-args)))))
  (printer args))

(define (memq item sequence)
  (cond ((null? sequence) #f)
        ((eq? item (car sequence)) sequence)
        (else (memq item (cdr sequence)))))

(main)
