(ns functional-data-structures.unbalanced-set-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.unbalanced-set :refer :all ]
            [functional-data-structures.set :refer :all ]))

(deftest test-is-member?
  (testing "testing one level"
    (let [one-level (unbalanced-set 1)]
      (is (is-member? one-level 1))
      (is (not (is-member? one-level 2)))
      (is (not (is-member? one-level -1)))))
  (testing "two levels"
    (let [two-level
          (unbalanced-set
            (unbalanced-set 1)
            2
            (unbalanced-set 3))]
      (is (is-member? two-level 1))
      (is (is-member? two-level 2))
      (is (is-member? two-level 3))
      (is (not (is-member? two-level 4)))
      (is (not (is-member? two-level -1))))
    ))

(deftest test-insert
  (testing "one"
    (is (= (insert nil 1) (unbalanced-set 1))))
  (testing "multiple inserts"
    (is (= (-> nil (insert 3) (insert 1)) (unbalanced-set (unbalanced-set 1) 3 nil)))
    (is (= (-> nil (insert 3) (insert 1) (insert 4)) (unbalanced-set (unbalanced-set 1) 3 (unbalanced-set 4))))
    (is (= (-> nil (insert 1) (insert 2) (insert 4)) (unbalanced-set nil 1 (unbalanced-set nil 2 (unbalanced-set 4)))))
    (is (= (-> nil (insert 1) (insert 3) (insert 2)) (unbalanced-set nil 1 (unbalanced-set (unbalanced-set 2) 3 nil))))
    ))

(deftest test-insert-member?
  (testing "one level"
    (let [one-level (-> nil (insert 1))]
      (is (is-member? one-level 1))
      (is (not (is-member? one-level 2)))
      (is (not (is-member? one-level -1)))))
  (testing "two levels"
    (let [two-level
          (->
            nil
            (insert 2)
            (insert 1)
            (insert 3))]
      (is (is-member? two-level 1))
      (is (is-member? two-level 2))
      (is (is-member? two-level 3))
      (is (not (is-member? two-level 4)))
      (is (not (is-member? two-level -1)))))
  (testing "random inserts"
    (->>
      (range 1 11)
      (map
        (fn [i]
          (let [test-tree (reduce #(insert % %2) nil (shuffle (range i)))]
            (->>
              (map
                #(is (is-member? test-tree %) true)
                (range i))
              doall))))
      doall)
    ))

(deftest test-smart-member?
  (testing "one level"
    (let [one-level (unbalanced-set 1)]
      (is (smart-member? one-level 1))
      (is (not (smart-member? one-level 2)))
      (is (not (smart-member? one-level -1)))))
  (testing "two levels"
    (let [two-level
          (unbalanced-set
            (unbalanced-set 1)
            2
            (unbalanced-set 3))]
      (is (smart-member? two-level 1))
      (is (smart-member? two-level 2))
      (is (smart-member? two-level 3))
      (is (not (smart-member? two-level 4)))
      (is (not (smart-member? two-level -1))))
    ))