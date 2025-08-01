Given lists:  
`(define x (list 1 2 3))`  
`(define y (list 4 5 6))`  

What result is printed by the interpreter in response to evaluating each of the following expressions:  

1. `(append x y)`  
   `(1 2 3 4 5 6)`  

2. `(cons x y)`  
   `((1 2 3) . (4 5 6))`  
   OR  
   `((1 2 3) 4 5 6)`  
   NOTE: the printer avoids dotted pair notation whenever it can, which is why my mit-scheme interpreter prints the second version.

3. `(list x y)`  
   `((1 2 3) (4 5 6))`  

