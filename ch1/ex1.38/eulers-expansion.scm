;; Utilize 'cont-fract-iter' to approximate e based on Euler's expansion.

(require racket/trace)

(define (main)
  (+ 2 (cont-fract-iter (lambda (i) 1.0)
	           d
	           10)))

;; Tips for solving the d Euler formula
;; - Notice how the sequence has a period of three, meaning the repeated pattern consists of three numbers.
;;   When this happens the solution usually involves using a modulus of 3.
;; - Apply the modulus, view the new sequence, take any notes of the current difference from the expected sequence.
;;   For example:
;;	Expected sequence:                   1 2 1 1 4 1 1 6 1  1  8 ...
;;	Our original sequence based off 'i': 1 2 3 4 5 6 7 8 9 10 11 ...
;;      Our sequence after mod 3:            1 2 0 1 2 0 1 2 0  1  2 ...
;;      Notice how we have a pattern of 3 repeating numbers still.
;;      The position of each 2 is where our incrementing power of 2 should go: 2, 4, 6, 8 ...
;;      If we add one to each element before applying the modulus, we get:
;;	2 3 4 5 6 7 8 9 10 11 12 ...
;;	2 0 1 2 0 1 2 0  1  2  0 ...
;;      Now notice how the 0 is where our power of 2 should go. Any other number that doesn't equally divide by 3 we can set as 1:
;;	2 0 1 2 0 1 2 0  1  2  0 ...
;;	1 0 1 1 0 1 1 0  1  1  0 ...
;;      Now look how close we are to the expected sequence:
;;	1 0 1 1 0 1 1 0  1  1  0 ...
;;	1 2 1 1 4 1 1 6 1  1  8 ...
;;	All thats left is calculating that power of 2, notice how the expected power of 2 is equal to this formula from the 
;;      value of the i'th element + 1 from our previous sequence:
;;	2 3 4 5 6 7 8 9 10 11 12 ...
;;        1     2     3        4
;;      Now multiply each multiple by 2:
;;        1     2     3        4
;;        2     4     6        8
;;      Now place in our latest sequence and we have our answer:
;;	1 2 1 1 4 1 1 6  1  1  8 ...
;;
;;	To sum up:
;;	We started with a sequence of just the incrementing values of 'i'
;;	We added 1 to each value, and checked if each were evenly divisible by 3.
;;      For those that weren't, we replaced their values with 1.
;;      For those that were, we replaced their value with how many times 3 goes into their original value + 1.



(define (d i)
  (cond ((= i 0) 2)
	((= (remainder (+ i 1) 3) 0) (* (/ (+ i 1) 3) 2))
	(else 1)))

;; In an iterative process, we don't have any delayed operations.
;; So to build an iterative process for a k-term finite continued fraction, we have to start
;; at the bottom and build upwards (backwards) vs. how we did in the recursive process, we started
;; at the top and worked our way down, using delayed operations to complete the fraction.

(define (cont-fract-iter n d k)
  (define (iter i result)
    (if (= i 1)
      (/ (n i) result)
      (iter (- i 1) (+ (d (- i 1)) (/ (n i) result)))))
  (iter k (d k)))
