;; Testing for bst package
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
