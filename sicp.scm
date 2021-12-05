;; Chapter 3 Exercises
;; 3.1 accumulator
(define (make-accumulator init)
  (let ((acc init))
    (lambda (num)
      (begin (set! acc (+ acc num))
	     acc))))
