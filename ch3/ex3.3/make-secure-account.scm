(define (main)
  (define acc (make-account 100 'secret-password))
  (println ((acc 'secret-password 'withdraw) 40)) ;;90
  (println ((acc 'asdf 'withdraw) 50))) ;; Incorrect password

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
    (cond ((not (eq? attempted-password password)) (error "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT"
		       m))))
  dispatch)
