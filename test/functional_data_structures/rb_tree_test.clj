(ns functional-data-structures.rb-tree-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.rb-tree :refer :all ]))

(deftest test-member?
  (testing "one level"
    (let [one-level (make-rb-tree 1)]
      (is (member? one-level 1))
      (is (not (member? one-level 2)))
      (is (not (member? one-level -1)))))
  (testing "two levels"
    (let [two-level (make-rb-tree
                      :B
                      (make-rb-tree 1)
                      2
                      (make-rb-tree 3))]
      (is (member? two-level 1))
      (is (member? two-level 2))
      (is (member? two-level 3))
      (is (not (member? two-level 4)))
      (is (not (member? two-level -1))))
    ))
