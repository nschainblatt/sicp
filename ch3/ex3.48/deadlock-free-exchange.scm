;; Calls exchange with with account with the smallest id being operated on first.
;; This avoids deadlocks because if two exchanges occur concurrently between the same account, the account in each exchange
;; will be operated on at the same step, forcing one of them to wait on the other.
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer))
	(account1-id (account1 'id))
	(account2-id (account2 'id)))
    (if (< account1-id account2-id)
      ((serializer1 (serializer2 exchange)) account1 account2)
      ((serializer2 (serializer1 exchange)) account1 account2))))

(define (make-account-and-serializer balance)
  (let ((id (make-id))) ;; Unique id
    (define (withdraw amount)
      (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      ((eq? m 'balance) balance)
	      ((eq? m 'serializer) balance-serializer)
	      ((eq? m 'id) id) ;; Accessible by a appropriate message
	      (else (error "Unknown request: MAKE-ACCOUNT" m))))
      dispatch)))

(define global-serializer (make-serializer))
(define id-index 0)
;; Serialized id generator, ensuring that all ids returned are unique and increase by one each time, starting at 1.
(define make-id
  (global-serializer
    (lambda ()
      (set! id-index (+ id-index 1))
      id-index)))
