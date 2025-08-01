Give combinations of cars and cdrs that will pick 7 from each of the following lists:  

1. `(1 3 (5 7) 9)`  
Remember that a proper 'list' follows this pattern: `(cons 1 (cons 2 (cons 3 '())))`  
So for the list in #1, it is represented like so:  
`(cons 1 (cons 3 (cons (cons 5 (cons 7 '())) (cons 9 '()))))`  
We can access 7 with this combination of cars and cdrs:  
cdr: `(cons 3 (cons (cons 5 (cons 7 '())) (cons 9 '())))`  
cdr: `(cons (cons 5 (cons 7 '())) (cons 9 '()))`  
car: `(cons 5 (cons 7 '()))`  
cdr: `(cons 7 '())`  
car: `7`

2. `((7))`  
`(cons (cons 7 '()) '())`  
car: `(cons 7 '())`  
car: `7`

3. `(1 (2 (3 (4 (5 (6 7))))))`  
`(cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())) '())) '()))`  
NOTE: `(6 7)` is a proper list nested in this sequence.  
cdr: `(cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())) '())) '())`  
car: `(cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())) '()))`  
cdr: `(cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())) '())`  
car: `(cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '()))`  
cdr: `(cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())`  
car: `(cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '()))`  
cdr: `(cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())`  
car: `(cons 5 (cons (cons 6 (cons 7 '())) '()))`  
cdr: `(cons (cons 6 (cons 7 '())) '())`  
car: `(cons 6 (cons 7 '()))`  
cdr: `(cons 7 '())`  
car: `7`  
