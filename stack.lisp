(defparameter stack1 `(1 2 3 4 5))

(defun stack-empty (stack) (null stack))

(defun stack-push (stack x) (cons x stack))

(defun stack-pop (stack) (rest stack))
