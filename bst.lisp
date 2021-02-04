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

(defun find-leaf (value tree)
  ;; Find if a value exists in a tree
  (cond ((null tree) nil)
        ((atom tree) (= tree value))
        (t (or (find-leaf value (first tree))
               (find-leaf value (rest tree))))))

(defun find-depth (depth value tree)
  ;; Find the depth of a node if the value exists in the tree
  ;; The depth form sets the depth of the first node
  (cond ((null tree) nil)
        ((atom tree) (and (= tree value) depth))
        (t (or (find-depth depth value (first tree))
               (find-depth (+ 1 depth) value (second tree))
               (find-depth (+ 1 depth) value (third tree))))))

(defun preorder-with-depths (tree)
  ;; Return the depths of each value and the value in a
  ;; preorder list
  (loop for leaf in (preorder tree)
    collect (list (find-depth 1 leaf tree) leaf)))

(defun sort-by-depth (tree)
  (sort (preorder-with-depth tree) #'< :key #'first))

(defun print-leaf (leaf)
  (format t " ~A~%" leaf))

(defun pretty-print-bst (depth tree)
  (cond ((null tree) nil)
        ((atom tree) (print-leaf tree))
        (t (or (pretty-print-bst depth  (first tree))
               (pretty-print-bst (+ 1 depth)  (second tree))
               (pretty-print-bst (+ 1 depth)  (third tree))))))
;;  (dotimes (i (depth tree))
;;    (loop for depth-and-value in (preorder-with-depths tree)
;;          do (if (= (first depth-and-value)
;;                    (+ 1 i))
;;                (format t "~A " (second depth-and-value))
;;                 ))
;;    (format t "~%")))
