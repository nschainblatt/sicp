(define (main)
  (define acc (make-account 100 'secret-password))
  (println ((acc 'secret-password 'withdraw) 40)) ;;90
  ((acc 'asdf 'withdraw) 50)			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)  			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)  			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)  			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)  			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)  			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)  			  ;; Incorrect password
  ((acc 'asdf 'withdraw) 50)) 			  ;; Call the cops

(define (make-account balance password)
  (let ((invalid-login-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (inc-invalid-login-attempts)
      (set! invalid-login-attempts (+ invalid-login-attempts 1)))
    (define (call-the-cops)
	(lambda args (println "Calling the cops!")))
    (define (handle-incorrect-password)
      (inc-invalid-login-attempts)
      (if (> invalid-login-attempts 7)
	(call-the-cops)
	(lambda args (println "Incorrect password"))))
    (define (dispatch attempted-password m)
      (cond ((not (eq? attempted-password password)) (handle-incorrect-password))
	    ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request: MAKE-ACCOUNT"
			 m))))
    dispatch))
