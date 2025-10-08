(define (main)
  (define v (list 'a 'b 'c 'd))
  (define w (mystery v))
  (println v) ;; '(d c b a)
  (println w) ;; '(a)
  'done)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
	(set-cdr! x y)
	(loop temp x))))
  (loop x '()))


(define (println x)
  (display x) (newline))

(main)
