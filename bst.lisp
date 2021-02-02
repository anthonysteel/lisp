(defparameter tree1 `(8 (3 1 (6 4 7)) (10 nil (14 13 nil))))
;;(defparameter tree2 `(8 (3 (1 nil nil) (6 (4 nil nil) (7 nil nil)) (10 nil (14 13 nil)))))
;;(defparameter tree3 `(6 (3 1 5) (9 7 11)))

(defun bst-depth (tree)
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (let ((left-subtree (second tree))
                 (right-subtree (third tree)))
             (+ 1 (max (bst-depth left-subtree)
                       (bst-depth right-subtree)))))))

(defun bst-depth-and-value (lst tree depth)
  (if (atom tree)
      (append lst `((depth tree)))
      (let ((left-subtree (second tree))
            (right-subtree (third tree)))
        (progn (append lst `((depth (first tree))))
               (bst-depth-and-value (lst left-subtree (+ 1 depth)))
               (bst-depth-and-value (lst right-subtree (+ 1 depth)))))))

(defun power (x n)
  (cond ((= n 0) 1)
        (t (* x (power x (- n 1))))))

(defun count-anywhere (x lst)
  (cond ((eql x lst) 1)
        ((atom lst) 0)
        (t (+ (count-anywhere x (first lst))
              (count-anywhere x (rest lst))))))

(defun bst-pretty-print (tree)
  ;; Consider the bst
  ;; (8 (3 1 (6 4 7)) (10 nil (14 13 nil)))
  ;; Visually,
  ;;                   8
  ;;                /     \
  ;;               3      10
  ;;              / \       \
  ;;             1   6      14
  ;;                / \    /
  ;;               4   7  13
  ;; First print 8
  ;; then 3 and 10
  ;; then 1 6 and 14
  ;;
  ;; Create a list where each element in the list is a list of
  ;; two elements, the depth and the value. Example:
  ;; ((1 8) (2 3) (2 10) (3 1) (3 6) (3 14) (4 4) (4 7) (4 13))
  (bst-depth-and-value (nil tree 0)))


;; Tests
(defun test-bst-depth ()
  (let ((tree0 `(8))
        (tree1 `(8 (3 1 (6 4 7)) (10 nil (14 13 nil))))
        (tree2 `(8 (3 (1 nil nil) (6 (4 nil nil) (7 nil nil)) (10 nil (14 13 nil)))))
        (tree3 `(6 (3 1 5) (9 7 11)))
        (tree4 `(8 (3 (1 nil nil) (6 4 nil)) 10)))
    (if (and (= (bst-depth tree0) 1)
             (= (bst-depth tree1) 4)
             (= (bst-depth tree2) 4)
             (= (bst-depth tree3) 3)
             (= (bst-depth tree4) 4))
        t
        nil)))
