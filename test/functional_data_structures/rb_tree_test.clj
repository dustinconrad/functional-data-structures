(ns functional-data-structures.rb-tree-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.rb-tree :refer :all ]))

(defn black? [node]
  (or (empty? node) (= :B (:color node))))

(defn red? [node]
  (= :R (:color node)))

(defn verify-rb-tree [{color :color left :left value :value right :right :as tree}]
  (cond
    (empty? tree) 0
    (and (red? tree) (or (red? left) (red? right))) (throw (IllegalStateException. "Red-Black tree invariant violated: Red node with red child"))
    :else (let [left-count (verify-rb-tree left)
                right-count (verify-rb-tree right)
                current-value-count (if (black? tree) 1 0)]
            (if-not (= left-count right-count)
              (throw (IllegalStateException. "Red-Black tree invariants violated: different number of black nodes to empty node"))
              (+ current-value-count left-count)))))

(deftest test-member?
  (testing "one level"
    (let [one-level (make-rb-tree 1)]
      (is (= 0 (verify-rb-tree one-level)))
      (is (member? one-level 1))
      (is (not (member? one-level 2)))
      (is (not (member? one-level -1)))))
  (testing "two levels"
    (let [two-level (make-rb-tree
                      :B
                      (make-rb-tree 1)
                      2
                      (make-rb-tree 3))]
      (is (= 1 (verify-rb-tree two-level)))
      (is (member? two-level 1))
      (is (member? two-level 2))
      (is (member? two-level 3))
      (is (not (member? two-level 4)))
      (is (not (member? two-level -1))))
    ))

(deftest test-insert-member?
  (testing "random inserts"
    (->>
      (range 1 11)
      (map
        (fn [i]
          (let [test-tree (reduce #(insert % %2) nil (shuffle (range i)))]
            (do
              (doall
                (map
                  #(is (member? test-tree %))
                  (range i)))
              (is (verify-rb-tree test-tree))
              (is (not (member? test-tree -10)))
              (is (not (member? test-tree (inc i))))
              ))))
      doall)))


(deftest test-from-ord-list
  (testing "empty"
    (let [tree (from-ord-list nil)]
      (is (verify-rb-tree tree))
      (is (= tree nil))))
  (testing "size one"
    (let [coll (range 1)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree))
      (doall
        (map
          #(is (member? tree %))
          coll))))
  (testing "size two"
    (let [coll (range 2)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree) (str tree))
      (doall
        (map
          #(is (member? tree %))
          coll))))
  (testing "size three"
    (let [coll (range 3)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree))
      (doall
        (map
          #(is (member? tree %))
          coll))))
  (testing "size four"
    (let [coll (range 4)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree) (str tree))
      (doall
        (map
          #(is (member? tree %))
          coll))))
  (testing "larger sizes"
    (->>
      (range 1 30)
      (map
        (fn [i]
          (let [coll (range i)
                tree (from-ord-list coll)]
            (is (verify-rb-tree tree) (str "i:" i " tree: " tree ))
              (map
                #(is (member? tree %))
                coll))))
      doall))
  )