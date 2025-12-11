(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
	(hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
	(require (>= hsq ksq))
	(let ((k (sqrt ksq)))
	  (require (integer? k))
	  (list i j k))))))

;; Ben's solution is indeed more efficient because he eliminated the problem revolving variable k.
;; He reduced the problem size from 3 to 2.
;; Previously, if 'require' always failed, we would have to go through all possible k's, followed by j's, and then i's.
;; Now, since there are only two usages of problems with amb, we only have to go through all possible j's and i's.
