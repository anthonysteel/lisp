(defun power (x n)
  (cond ((= n 0) 1)
        (t (* x (power x (- n 1))))))

(defun count-anywhere (x lst)
  (cond ((eql x lst) 1)
        ((atom lst) 0)
        (t (+ (count-anywhere x (first lst))
              (count-anywhere x (rest lst))))))

(defparameter tree1 `(8 (3 1 (6 4 7)) (10 nil (14 13 nil))))
(defparameter tree2 `(8 (3 (1 nil nil) (6 (4 nil nil) (7 nil nil)) (10 nil (14 13 nil)))))
(defparameter tree3 `(6 (3 1 5) (9 7 11)))
(defparameter tree4 `(38 (5 1 (9 8 (15 13 nil))) (45 nil (47 46 nil))))


(defun depth (tree)
  ;; Find the maximum depth of the tree
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (let ((left-subtree (second tree))
                 (right-subtree (third tree)))
             (+ 1 (max (depth left-subtree)
                       (depth right-subtree)))))))

(defun preorder (tree)
  ;; Return the tree as a list in preorder
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (preorder (first tree))
                   (preorder (rest tree))))))

(defun find (value tree)
  ;; Find if a value exists in a tree
  (cond ((null tree) nil)
        ((atom tree) (= tree value))
        (t (or (find value (first tree))
               (find value (rest tree))))))

(defun find-depth (depth value tree)
  ;; Find the depth of a node if the value exists in the tree
  (cond ((null tree) nil)
        ((atom tree) (and (= tree value) depth))
        (t (or (find-depth depth value (first tree))
               (find-depth (+ 1 depth) value (second tree))
               (find-depth (+ 1 depth) value (third tree))))))
(defun preorder-with-depth (tree)
  (let (preorder-tree preorder(tree))))

;; Tests
(defun test-depth ()
  (let ((tree0 `(8))
        (tree1 `(8 (3 1 (6 4 7)) (10 nil (14 13 nil))))
        (tree2 `(8 (3 (1 nil nil) (6 (4 nil nil) (7 nil nil)) (10 nil (14 13 nil)))))
        (tree3 `(6 (3 1 5) (9 7 11)))
        (tree4 `(8 (3 (1 nil nil) (6 4 nil)) 10)))
    (if (and (= (depth tree0) 1)
             (= (depth tree1) 4)
             (= (depth tree2) 4)
             (= (depth tree3) 3)
             (= (depth tree4) 4))
        t
        nil)))
