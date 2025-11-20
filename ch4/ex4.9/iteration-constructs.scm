;; In general we can make iterative processes by using ordinary procedure calls last.
;; We can make the required iteration constructs by using procedures, namely make-lambda to make
;; these derived expressions.

;; These are examples of real iterative constructs, not the derived expressions. These are used as a mental model when
;; building the derived expressions for our language.

(define (println x)
  (newline)
  (display x))

(define (operation x)
  (println x)
  (+ x 1))

(define (reverse-operation x)
  (println x)
  (- x 1))

(define (predicate x)
  (< x 10))

(define (do-while operation predicate . initial-variables)
  (define (iter changed-variables)
    (if (predicate changed-variables)
      (iter (operation changed-variables))
      'done))
  (iter (apply operation initial-variables)))

; (do-while operation predicate 0)

(define (while operation predicate . initial-variables)
  (define (iter . changed-variables)
    (if (apply predicate changed-variables)
      (iter (apply operation changed-variables))
      'done))
  (apply iter initial-variables))

; (while operation predicate 0)

(define (until operation predicate . initial-variables)
  (define (inverted-predicate . variables)
    (not (apply predicate variables)))
  (define (iter . changed-variables)
    (if (apply inverted-predicate changed-variables)
      (iter (apply operation changed-variables))
      'done))
  (apply iter initial-variables))

; (until reverse-operation predicate 20)

(define (for initial predicate increment operation)
  (define (iter x)
    (if (predicate x)
      (begin (operation x) (iter (+ x increment)))
      'done))
  (iter initial))

(for 0 predicate 1 operation)

;; TODO: derived expressions for our language.
