(define (example a b c d)
  (let ((sum (+ a b c d))
	(product (* a b c d))
	(quotient (/ a b c d)))
    (println sum)
    (println product)
    (println quotient)
    'done))

(define (example-iter start end f)
  (if (> start end)
    'done
    (begin (f start) (example-iter (+ start 1) end f))))
