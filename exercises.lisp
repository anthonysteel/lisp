;; Exercises from PAIP
;; See github.com/norvig/paip-lisp

;; E1.2
(defun power (x n)
  (cond ((= n 0) 1)
        (t (* x (power x (- n 1))))))

;; E1.4
(defun count-anywhere (x lst)
  (cond ((eql x lst) 1)
        ((atom lst) 0)
        (t (+ (count-anywhere x (first lst))
              (count-anywhere x (rest lst))))))
