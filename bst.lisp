;;(defparameter tree1 `(8 (3 1 (6 4 7)) (10 nil (14 13 nil))))
;;(defparameter tree2 `(8 (3 (1 nil nil) (6 (4 nil nil) (7 nil nil)) (10 nil (14 13 nil)))))
;;(defparameter tree3 `(6 (3 1 5) (9 7 11)))

(defun bst-print (tree)
  (print (list-length tree)))

(defun bst-depth (tree)
  (if (null tree)
    0
    (if (atom tree)
        1
        (let ((left-subtree (second tree))
              (right-subtree (third tree)))
          (+ 1 (max (bst-depth left-subtree)
                    (bst-depth right-subtree)))))))


(defun bst-add (tree value)
  (if (null tree)
      `(,value nil nil)
      (if (> value (first tree))
          (rest tree)
          (first tree))))

;; Tests
(defun test-bst-depth ()
  (let ((tree1 `(8 (3 1 (6 4 7)) (10 nil (14 13 nil))))
        (tree2 `(8 (3 (1 nil nil) (6 (4 nil nil) (7 nil nil)) (10 nil (14 13 nil)))))
        (tree3 `(6 (3 1 5) (9 7 11)))
        (tree4 `(8 (3 (1 nil nil) (6 4 nil)) 10)))
    (if (and (= (bst-depth tree1) 4)
             (= (bst-depth tree2) 4)
             (= (bst-depth tree3) 3)
             (= (bst-depth tree4) 4))
        t
        nil)))
