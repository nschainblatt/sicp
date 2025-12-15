;; Write a non-deterministic procedure that will solve the eight-queens puzzle.
;; Instead of generating all possible solutions upfront, it will generate a single solution at a time.
;; You can repeatedly call try-again which will generate a new solution.

(define (queens board-size)

  ;; Returns a list of all possible positions a queen can be in a row, with the index stored in the choice pair.
  (define row-choices (let ((interval (enumerate-interval 0 (dec board-size))))
			(map (lambda (index)
			       (make-row-choice-pair index (make-row-with-queen-at index interval)))
			     interval)))

  (define (queen-row-iter k board)
    (if (= k 0)
      board
      (let ((row-choice-pair (amb row-choices))) ;; Contains index of queen & row.
	(require (safe? (dec k) (choice-queen-index row-choice-pair) board)) ;; dec to get 0 based index.
	(queen-row-iter (dec k) (append board (choice-row row-choice-pair))))))

  (queen-row-iter board-size '()))


(define (enumerate-interval start end)
  (if (> start end)
    '()
    (cons start (enumerate-interval (+ start 1) end))))


;; Returns a row with a queen at the index.
(define (make-row-with-queen-at index interval)
  (map (lambda (j) 
	 (if (= index j)
	   "Q" 
	   " ")) interval))

;; Ensure the position at row-index/col-index is a safe place to put a new queen.
;; It must not be in the same row, column or diagonal as another queen on the board.
(define (safe? new-row-index new-col-index board)
  (define (row-iter current-row-index rows)
    (if (null? rows)
      #t
      (let ((row (car rows))
	    (board-size (length row))
	    (right-diagonal-index (+ (- new-row-index current-row-index) new-col-index))
	    (left-diagonal-index (- (- new-row-index current-row-index) new-col-index)))
	(if (or (queen? (list-ref row new-col-index))
		(and (not (out-of-bounds? row right-diagonal-index)) (queen? (list-ref row right-diagonal-index))) ;; Note: out of bounds is safe
		(and (not (out-of-bounds? row left-diagonal-index)) (queen? (list-ref row left-diagonal-index))))
	  #f
	  (row-iter (inc current-row-index) (cdr rows)))))

    (row-iter 0 board))

  (define (queen? x)
    (equal? x "Q"))

  (define (inc x)
    (+ x 1))

  (define (dec x)
    (- x 1))

  (define (out-of-bounds? seq index)
    (or (> index (- (length seq) 1))
	(< index 0)))

  (define (make-row-choice-pair index-of-queen row)
    (cons index-of-queen row))
  (define (choice-queen-index p)
    (car p))
  (define (choice-row p)
    (cdr p))
