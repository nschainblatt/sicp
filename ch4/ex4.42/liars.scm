;; ((betty 1) (ethel 5) (joan 3) (kitty 2) (mary 4))
(define (liars)
  (let ((betty-ref 0)
	(ethel-ref 1)
	(joan-ref 2)
	(kitty-ref 3)
	(mary-ref 4)
	(perms (permutations (list 1 2 3 4 5))))
    (filter (lambda (seq)
	      (and
		(xor (= (list-ref seq kitty-ref) 2) (= (list-ref seq betty-ref) 3))
		(xor (= (list-ref seq ethel-ref) 1) (= (list-ref seq joan-ref) 2))
		(xor (= (list-ref seq joan-ref) 3) (= (list-ref seq ethel-ref) 5))
		(xor (= (list-ref seq kitty-ref) 2) (= (list-ref seq mary-ref) 4))
		(xor (= (list-ref seq mary-ref) 4) (= (list-ref seq betty-ref) 1))))
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

(define (xor x y)
  (or (and (not x) y)
       (and x (not y))))

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

(println (liars))
