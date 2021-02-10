(defun get-step-size (start end steps)
  (/ (- end start) steps))

(defun make-list-of-steps (start end steps)
  (let ((step-size (get-step-size start end steps))
        (list ()))
    (dotimes (n steps (nreverse list)) (push (+ start (* n step-size)) list)))))

(defun 0-to-2pi (steps)
  (make-list-of-steps 0 (* 2 pi) steps))

(defparameter current (map 'list (lambda (i) (* 2 (sin i))) (0-to-2pi 100)))
(defparameter voltage (map 'list (lambda (i) (sin (+ i (/ pi 2)))) (0-to-2pi 100)))

(defun rms-power (ac-voltage ac-current)
  (assert (= (length ac-voltage) (length ac-current)))
  (let ((time-interval (length ac-voltage)))
    (reduce '+ (loop for voltage in ac-voltage
          for current in ac-current
          collect (/ (* voltage current) time-interval)))))
