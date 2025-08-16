;; NOTE: 1 is a queen 0 is empty space. An empty board looks like this for a 3x3 board:
;; (((0),(0),(0))
;;  ((0),(0),(0))
;;  ((0),(0),(0)))

(define (main)
  (let ((safe-positions (queens 6)))
    (foreach (lambda (x) (print-board x) (newline)) safe-positions)
    (display (length safe-positions))
    (newline)))

;; Louis's version moved queen-cols to the inner most map,
;; which increases it's call count to by a multiple of the board-size.
;; These extra calls aren't necessary, and as seen in the original
;; queens procedure definition, the call to queen-cols is the sequence
;; given to the outer flatmap, where it is only calculated once.
;; This affects the time complexity because the computationally heavy
;; procedure queen-cols is moved down into another map. The new time complexity
;; would have to reflect this by making it the  T*board-size^board-size, where T is the
;; time complexity of the original queens procedure.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list (empty-board board-size))
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (empty-board board-size)
  (map (lambda (row)
         (map (lambda (col) (list 0))
              (enumerate-interval 1 board-size)))
       (enumerate-interval 1 board-size)))

;; NOTE: positions is really just a single board
(define (safe? k positions)
  (cond ((= k 1) #t) ;; First column placement is always safe
        ((is-checked? k positions) #f)
        (else #t)))

(define (is-checked? k positions)
  (let ((new-queen-row (find-row-of-queen-in-column-k k positions)))
    (has-queen-in-same-row-or-diagonal? positions new-queen-row k)))

;; Returns the row number where the new queen is positioned in column k.
;; This is determined by iterating through each row, iterating to column k within
;; that row, and returning the row number if a queen is found. Note that this will return
;; the position of the first queen encountered in column k. An error will be raised if
;; no queen was found in column k for all rows.
(define (find-row-of-queen-in-column-k k positions)
  ;; Checks if the k col of the current row is a queen
  (define (col-iter col-index sub-pos)
    (cond ((null? sub-pos) -1)
          ((and (= col-index k) (= (car (car sub-pos)) 1)) col-index)
          (else (col-iter (+ col-index 1) (cdr sub-pos)))))

  ;; Goes over every row to check column k for a queen
  (define (row-iter row-index sub-pos)
    (cond ((null? sub-pos) (error "didn't find a queen in column k"))
          ((not (= (col-iter 1 (car sub-pos)) -1)) row-index)
          (else (row-iter (+ row-index 1) (cdr sub-pos)))))

  (row-iter 1 positions))

;; Returns true if a queen is found in the same row as the new queen in column k or there is a queen diagonal to the new queen
;; in column k.
;; This is determined by iterating over every row in the current boards positions, if the row is the same row as the queen in column k,
;; then we iterate through the columns of that row checking for other queens. During the same row iteration, we check each rows
;; columns that are diagonal to the new queen in column k to see if they contain queens as well.
;; We return as soon as a queen is found that would check the new queen in column k.
(define (has-queen-in-same-row-or-diagonal? positions new-queen-row new-queen-col)

  (define (row-iter row-index sub-pos)

    (define (same-row-col-iter col-index sub-row)
      (cond ((null? sub-row) (error "out of bounds, we should've returned early or found a queen by now"))
            ((= col-index new-queen-col) #f) ;; Reached new-queen-col, safe
            ((= (car (car sub-row)) 1) #t) ;; Found a queen before new-queen-col, not safe
            (else (same-row-col-iter (+ col-index 1) (cdr sub-row)))))

    (define (diag-col-iter col-index sub-row)
      (cond ((null? sub-row) (error "out of bounds, we should've returned early or found a queen by now"))
            ((= col-index new-queen-col) #f) ;; Reached new-queen-col, safe
            ((and (is-diagonal? new-queen-row new-queen-col row-index col-index) (= (car (car sub-row)) 1)) #t)
            (else (diag-col-iter (+ col-index 1) (cdr sub-row)))))

    (cond ((null? sub-pos) #f)
          ((= row-index new-queen-row) (if (same-row-col-iter 1 (car sub-pos))
                                         #t
                                         (row-iter (+ row-index 1) (cdr sub-pos))))
          ((diag-col-iter 1 (car sub-pos)) #t)
          (else (row-iter (+ row-index 1) (cdr sub-pos)))))

  (row-iter 1 positions))

(define (is-diagonal? row1 col1 row2 col2)
  (or (= (+ row1 col1) (+ row2 col2)) (= (/ (- row2 row1) (- col2 col1)) 1)))

(define (adjoin-position new-row k rest-of-queens)
  (map-with-index
    (lambda (row row-pointer)
      (map-with-index
        (lambda (col col-pointer)
          (cond ((and (= col-pointer k) (= row-pointer new-row)) (list 1))
                (else col)))
        row))
    rest-of-queens))

(define (flatmap op sequence)
  (accumulate append '() (map op sequence)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate sequence))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval start end)
  (if (> start end)
    '()
    (cons start (enumerate-interval (+ start 1) end))))

(define (foreach proc sequence)
  (define (iter sub-seq)
    (cond ((null? sub-seq)(display ""))
          (else (proc (car sub-seq)) (iter (cdr sub-seq)))))
  (iter sequence))

(define (print-board board)
  (define (iter sub-board)
    (cond ((null? sub-board) (display ""))
          (else
            (display (car sub-board))
            (newline)
            (iter (cdr sub-board)))))
  (iter board))

(define (map-with-index proc sequence)
  (define (iter sub-sequence result index)
    (if (null? sub-sequence)
      result
      (iter (cdr sub-sequence) (append result (list (proc (car sub-sequence) index))) (+ index 1))))
  (iter sequence '() 1))

(define (enumerate-interval start end)
  (if (> start end)
    '()
    (cons start (enumerate-interval (+ start 1) end))))
