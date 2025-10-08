(define (main)
  (define x (list 'a 'b))
  (define y (list 'c 'd))
  (define z (append x y))
  (println z)
  (println (cdr x))
  (define w (append! x y))
  (println w)
  (println (cdr x))
  'done)

(define (append! x y)
  (set-cdr! (last-pair x) y) x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (println x)
  (display x) (newline))

(main) ;; Racket has seperate mutable data structures not aligned with mit-scheme, so run this with a mit-scheme interpreter.
