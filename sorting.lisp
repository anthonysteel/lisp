(defparameter arr `(43 14 15 1123 -218 11 -10 14113))

(defun partition (arr left pivot right)
  (cond ((null arr) (values left pivot right))
        ((> (first arr) (first pivot))
         (partition (rest arr) left pivot (cons (first arr) right)))
        ((<= (first arr) (first pivot))
         (partition (rest arr) (cons (first arr) left) pivot right))))

(defun quicksort (arr)
  (cond ((null arr) nil)
        (t (multiple-value-bind (left pivot right) (partition (butlast arr) nil (last arr) nil)
            (append (append (quicksort left) pivot) (quicksort right))))))
