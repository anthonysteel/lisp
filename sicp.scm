;; Chapter 3 Exercises
;; 3.1 accumulator
(define (make-accumulator init)
  (let ((acc init))
    (lambda (num)
      (begin (set! acc (+ acc num))
	     acc))))

;; 3.2 make-monitored
(define (make-monitored f)
  (let ((i 0) (fun f))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) i)
	    ((eq? m 'reset-count) (begin (set! i 0) i))
	    ((number? m) (begin (set! i (+ i 1)) (fun m)))
	    (else (error "Unknown request -- MAKE-MONITORED" m))))
    mf))
