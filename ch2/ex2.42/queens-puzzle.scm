;; NOTE: 1 is a queen 0 is empty space. An empty board looks like this for a 3x3 board:
;; (((0),(0),(0))
;;  ((0),(0),(0))
;;  ((0),(0),(0)))

(define (main)
  (let ((safe-positions (queens 8)))
    (foreach (lambda (x) (print-board x) (newline)) safe-positions)
    (display (length safe-positions))
    (newline)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list (empty-board board-size))
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
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
  (let ((row-of-k-queen (find-row-of-queen-in-column-k k positions)))
    (or (has-queen-in-same-row? k positions row-of-k-queen)
        (has-queen-in-same-col? k positions row-of-k-queen)
        (has-queen-in-diagonal? k positions row-of-k-queen))))

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

;; Returns true if a queen is found in the same row as the new queen on column k.
;; This is determined by iterating through the board to find the correct row,
;; then iterating through the columns of that row until k, if a queen is found
;; before reaching column k, then we return true. If k is reached and no queen has been found,
;; then we return false because k is the latest modified column. No columns past k have any queens yet.
(define (has-queen-in-same-row? k positions row-of-k-queen)

  (define (col-iter index sub-row)
    (cond ((null? sub-row) (error "out of bounds, we should've returned early or found a queen by now"))
          ((= index k) #f) ;; Reached k, safe
          ((= (car (car sub-row)) 1) #t) ;; Found a queen before k, not safe
          (else (col-iter (+ index 1) (cdr sub-row)))))

  ;; Finds the row of row-of-k-queen, then calls col-iter to find a queen if any.
  (define (row-iter index sub-pos)
    (cond ((null? sub-pos) (error "didn't find correct row"))
          ((= index row-of-k-queen) (col-iter 1 (car sub-pos)))
          (else (row-iter (+ index 1) (cdr sub-pos)))))

  (row-iter 1 positions))

(define (has-queen-in-same-col? k positions row-of-k-queen)
  ;;NOTE: returning false always, since we are adding a new queen to column k, there will already be no other queens in that column
  #f)

;; Returns true if there is a queen diagonal to the new queen in column k.
;; This is determined by iterating through each row in the board, and for each
;; column in each row, we check if the column is diagonal to the position of the queen
;; in column k, and then we check if it has a queen in it.
(define (has-queen-in-diagonal? k positions row-of-k-queen)
  ;; Go through each row in the board
  (define (row-iter row-index sub-pos)
    ;; Go through each column in the row
    ;; Check if the current cell is diagonal to the new position of the queen to place
    ;; Return early if a queen is found, or we've reached k in the current row (can skip if the case)
    (define (col-iter col-index sub-row)
      (cond ((null? sub-row) (error "out of bounds, we should've returned early or found a queen by now"))
            ((= col-index k) #f) ;; Reached k, safe
            ((and (is-diagonal? row-of-k-queen k row-index col-index) (= (car (car sub-row)) 1)) #t)
            (else (col-iter (+ col-index 1) (cdr sub-row)))))

    (cond ((null? sub-pos) #f)
          ((= row-index row-of-k-queen) (row-iter (+ row-index 1) (cdr sub-pos))) ;; Continue, no need to check same row
          ((col-iter 1 (car sub-pos)) #t)
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
