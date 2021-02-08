(defparameter arr `(43 14 15 7 8 11 -10 13))

;; (quicksort (butlast arr) (last arr))
;;(defun quicksort (below-pivot above-pivot)
;;)

(defun pivot-sort (arr left pivot right)
  (cond ((null arr) (append (append left pivot) right))
        ((> (first arr) (first pivot))
         (pivot-sort (rest arr) left pivot (cons (first arr) right)))
        ((<= (first arr) (first pivot))
         (pivot-sort (rest arr) (cons (first arr) left) pivot right))))
