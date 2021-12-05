;; Chapter 3 Exercises
;; 3.1 accumulator
(define (make-accumulator init)
  (let ((acc init))
    (lambda (num)
      (begin (set! acc (+ acc num))
	     acc))))

;; 3.2 make-monitored
(define (make-monitored f)
  (let ((i 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) i)
	    ((eq? m 'reset-count) (begin (set! i 0) i))
	    ((number? m) (begin (set! i (+ i 1)) (f m)))
	    (else (error "Unknown request -- MAKE-MONITORED" m))))
    mf))

;; 3.3 password protected make-account
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	"Incorrect password"))
  dispatch) ;; error with how incorrect password is being returned




