(defparameter A '(0 16 4 10 14 7 9 3 2 8 1))
(setf A '(0 16 4 10 14 7 9 3 2 8 1))

(defun parent (i) (/ i 2))

(defun left (i) (* 2 i))

(defun right (i) (+ (* 2 i) 1))

(defun heap-size (A)
  (- (length A) 1))


(defun swap (i j A)
  (rotatef (nth i A) (nth j A)))

(defun max-heapify (A i)
  (let ((l (left i))
	(r (right i)))
    (let ((largest (cond ((and (<= l (heap-size A))
			      (> (nth l A) (nth i A))) l)
			  (t i))))
      (if (and (<= r (heap-size A))
	       (> (nth r A) (nth largest A)))
	  (setf largest r))
      (if (not (eq largest i))
	  (progn (swap i largest A)
		 (max-heapify A largest))))))

