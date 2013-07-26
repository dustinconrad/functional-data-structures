(ns functional-data-structures.bst-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.bst :refer :all ]))

(deftest test-make-tree
  (testing "make a tree"
    (let [expected {:left "left" :value "value" :right "right"}
          actual (make-tree "left" "value" "right")]
      (is (= actual expected)))))

(deftest test-member?
  (testing "one level"
    (let [one-level (make-tree 1)]
      (is (member? one-level 1))
      (is (not (member? one-level 2)))
      (is (not (member? one-level -1)))))
  (testing "two levels"
    (let [two-level (make-tree
                      (make-tree 1)
                      2
                      (make-tree 3))]
      (is (member? two-level 1))
      (is (member? two-level 2))
      (is (member? two-level 3))
      (is (not (member? two-level 4)))
      (is (not (member? two-level -1))))
    ))

(deftest test-insert
  (testing "one"
    (is (= (insert nil 1) (make-tree 1))))
  (testing "multiple inserts"
    (is (= (-> nil (insert 3) (insert 1)) (make-tree (make-tree 1) 3 nil)))
    (is (= (-> nil (insert 3) (insert 1) (insert 4)) (make-tree (make-tree 1) 3 (make-tree 4))))
    (is (= (-> nil (insert 1) (insert 2) (insert 4)) (make-tree nil 1 (make-tree nil 2 (make-tree 4)))))
    (is (= (-> nil (insert 1) (insert 3) (insert 2)) (make-tree nil 1 (make-tree (make-tree 2) 3 nil))))
    ))

(deftest test-insert-member?
  (testing "random inserts"
    (->>
      (range 1 11)
      (map
        (fn [i]
          (let [test-tree (reduce #(insert % %2) nil (shuffle (range i)))]
            (->>
              (map
                #(is (= (member? test-tree %) true))
                (range i))
              doall))))
      doall)))