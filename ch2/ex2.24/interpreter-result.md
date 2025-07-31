1. Interpreter result:  
```(1 (2 (3 4)))```

2. Box and pointer result  
```
[∙|∙]――>[∙|x]
 |       |
 v       v
 1     [∙|∙]――>[∙|x]
        |       |
        v       v
        2     [∙|∙]――>[∙|x]
               |       |
               v       v
               3       4
```


3. Tree result  
```
(1 (2 (3 4)))
      ∙
     / \ (2 (3 4))
    1   ∙
       / \ (3 4)
      2   ∙
         / \
        3   4
```
