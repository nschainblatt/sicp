```scm
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude) ;; Putting the generic magnitude inside the complex package, not the one from rectangular
(put 'angle '(complex) angle)
```

The reason why installing these procedures makes Louis's call to 'magnitude' work is that
the procedure magnitude did not exist inside the operation-type-table for the 'complex' type.
Adding these procedures allows the 'complex' type to be stripped and the actual implementation, (rectangular in this case) to be called,
returning the expected answer.

```scm
(define (apply-generic op obj)
  (apply (get op (get-type obj)) (get-contents obj)))

;; Generic magnitude selector
(define (magnitude z)
  (apply-generic 'magnitude z))

;; Trace
(magnitude z) ;; NOTE: this is calling the above generic
(apply-generic 'magnitude z)
(apply (get 'magnitude (get-type z)) (get-contents z))
(apply (get 'magnitude 'complex) ('rectangular (3 4)))
(apply magnitude ('rectangular (3 4))) ;; NOTE: this magnitude is the same generic one called before that we installed at the top
(magnitude ('rectangular (3 4)))
(apply-generic 'magnitude ('rectangular (3 4)))
(apply (get 'magnitude (get-type ('rectangular (3 4)))) (get-contents ('rectangular (3 4))))
(apply (get 'magnitude 'rectangular) (3 4))
(apply magnitude (3 4)) ;; NOTE: this magnitude is the one from the rectangular package
(magnitude (3 4)) ;; NOTE: this magnitude is the one from the rectangular package
(sqrt (+ (square (real-part (3 4)))
         (square (imag-part (3 4)))))
(sqrt (+ (square 3)
         (square 4)))
(sqrt (+ 9 16))
(sqrt 25)
5
```

'apply-generic' is applied twice, each time trimming off the type information and calling the specific magnitude for the type, once
for complex and another for the rectangular.
The magnitude procedure stored in the complex package simply re-referenced the same generic magnitude defined globally,
this one strips off type information, leading to the rectangular type underneath which is where the calculation is really performed and 5
is returned.
