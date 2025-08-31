(define (main)
  (let ((sample-message '(A D A B B C A))
        (sample-tree
          (make-code-tree (make-leaf 'A 4)
                          (make-code-tree
                            (make-leaf 'B 2)
                            (make-code-tree
                              (make-leaf 'D 1)
                              (make-leaf 'C 1))))))
    (decode (encode (decode (encode  sample-message sample-tree) sample-tree) sample-tree) sample-tree)))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol)) '())
        ((and (not (leaf? tree)) (symbol-in-set? symbol (symbolz (left-branch tree)))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((and (not (leaf? tree)) (symbol-in-set? symbol (symbolz (right-branch tree)))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not found in code tree"))))

(define (symbol-in-set? symbol set)
  (cond ((null? set) #f)
        ((eq? (car set) symbol) #t)
        (else (symbol-in-set? symbol (cdr set)))))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbolz tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbolz left) (symbolz right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))
(define (right-branch tree) (cadr tree))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))
