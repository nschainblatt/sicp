The order of growth as a function of n when given one of the trees from ex2.71 and trying to encode the most frequent symbol is
O(1) because we simply have to take a single left branch to a set of just one symbol to search.

For the least frequent symbol, the order of grown is O(n^2), because with the trees in ex2.71, we have to scan the left branches symbols,
which only contains one symbol, as well as the right branch which contains the rest of the symbols, equaling a linear search each step of the
way totaling to quadratic complexity.
```scm
(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol)) '())
        ((and (not (leaf? tree)) (symbol-in-set? symbol (symbolz (left-branch tree)))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((and (not (leaf? tree)) (symbol-in-set? symbol (symbolz (right-branch tree)))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not found in code tree"))))
```
