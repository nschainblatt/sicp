(define (main)
  ;; This will display 'quote' because ''abc will produce a list: (quote abc).
  ;; A list is created because the first quote is expanded to expression: (quote abc),
  ;; then when the second quote is applied this expression is produced: (quote (quote abc)).
  ;; When this is evaluated a literal list of (quote abc) is produced.
  ;; Then when car is applied to the list, 'quote' is returned.
  (car ''abc))
