;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
(define (multiple-dwelling)
  (let ((baker-ref 0)
	(cooper-ref 1)
	(fletcher-ref 2)
	(miller-ref 3)
	(smith-ref 4)
	(perms (permutations (list 1 2 3 4 5))))
    (filter (lambda (seq)
	      (and
		(not (= (list-ref seq baker-ref) 5))
		(not (= (list-ref seq cooper-ref) 1))
		(not (= (list-ref seq fletcher-ref) 1))
		(not (= (list-ref seq fletcher-ref) 5))
		(> (list-ref seq miller-ref) (list-ref seq cooper-ref))
		(not (= (abs (- (list-ref seq smith-ref) (list-ref seq fletcher-ref))) 1))
		(not (= (abs (- (list-ref seq cooper-ref) (list-ref seq fletcher-ref))) 1))))
	    perms)))

(define (permutations s)
  (if (null? s) ; empty set?
    (list '()) ; sequence containing empty set
    (flatmap (lambda (x)
	       (map (lambda (p) (cons x p))
		    (permutations (remove x s))))
	     s)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (accumulate proc initial seq)
  (if (null? seq)
    initial
    (proc (car seq) (accumulate proc initial (cdr seq)))))

(define (println . args)
  (newline)
  (define (init a)
    (if (null? a)
      'done
      (begin (display (car a)) (display " ") (init (cdr a)))))
  (init args))

(println (multiple-dwelling))
