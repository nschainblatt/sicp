(define (main)
  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

  (println ((peter-acc 'open-sesame 'withdraw) 40)) ;; 60
  (println ((paul-acc 'rosebud 'withdraw) 50))      ;; 10

  (println ((peter-acc 'rosebud 'withdraw) 40))     ;; Incorrect password
  (println ((paul-acc 'open-sesame 'withdraw) 50)))  ;; "

(define (make-joint acc acc-password joint-acc-password)
  (lambda (attempted-password m)
    (if (eq? attempted-password joint-acc-password)
      (acc acc-password m)
      (error "Incorrect password: MAKE-JOINT-ACCOUNT"))))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch attempted-password m)
    (cond ((not (eq? attempted-password password)) (error "Incorrect password: MAKE-ACCOUNT"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

