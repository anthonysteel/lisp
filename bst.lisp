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


(defun bst-depth (tree)
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (let ((left-subtree (second tree))
                 (right-subtree (third tree)))
             (+ 1 (max (bst-depth left-subtree)
                       (bst-depth right-subtree)))))))

(defun bst-preorder (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (bst-preorder (first tree))
                   (bst-preorder (rest tree))))))

(defun bst-find (value tree)
  (cond ((null tree) nil)
        ((atom tree) (= tree value))
        (t (or (bst-find value (first tree))
               (bst-find value (rest tree))))))

(defun bst-find-depth (depth value tree)
  (cond ((null tree) nil)
        ((atom tree) (and (= tree value) depth))
        (t (or (bst-find-depth depth value (first tree))
               (bst-find-depth (+ 1 depth) value (second tree))
               (bst-find-depth (+ 1 depth) value (third tree))))))


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
