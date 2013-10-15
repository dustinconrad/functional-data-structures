(ns functional-data-structures.rb-tree-test
  (:require [clojure.test :refer :all]
            [functional-data-structures.set :refer :all]
            [functional-data-structures.rb-tree :refer :all ]))

(defn black? [node]
  (or (is-empty? node) (= :B (:color node))))

(defn red? [node]
  (= :R (:color node)))

(defn verify-rb-tree [{color :color left :left value :value right :right :as tree}]
  (cond
    (is-empty? tree) 0
    (and (red? tree) (or (red? left) (red? right))) (throw (IllegalStateException. "Red-Black tree invariant violated: Red node with red child"))
    :else (let [left-count (verify-rb-tree left)
                right-count (verify-rb-tree right)
                current-value-count (if (black? tree) 1 0)]
            (if-not (= left-count right-count)
              (throw (IllegalStateException. (str "Red-Black tree invariants violated: different number of black nodes to empty node (" left-count "," right-count ")")))
              (+ current-value-count left-count)))))

(deftest test-member?
  (testing "one level"
    (let [one-level (->RedBlackSet :R (->RedBlackSet :B nil nil nil) 1 (->RedBlackSet :B nil nil nil))]
      (is (= 0 (verify-rb-tree one-level)))
      (is (is-member? one-level 1))
      (is (not (is-member? one-level 2)))
      (is (not (is-member? one-level -1)))))
  (testing "two levels"
    (let [two-level (->RedBlackSet
                      :B
                      (->RedBlackSet :R (->RedBlackSet :B nil nil nil) 1 (->RedBlackSet :B nil nil nil))
                      2
                      (->RedBlackSet :R (->RedBlackSet :B nil nil nil) 3 (->RedBlackSet :B nil nil nil)))]
      (is (= 1 (verify-rb-tree two-level)))
      (is (is-member? two-level 1))
      (is (is-member? two-level 2))
      (is (is-member? two-level 3))
      (is (not (is-member? two-level 4)))
      (is (not (is-member? two-level -1))))
    ))

(deftest test-insert-member?
  (testing "random inserts"
    (->>
      (range 1 11)
      (map
        (fn [i]
          (let [test-tree (reduce #(insert % %2) (red-black-set) (shuffle (range i)))]
              (doall
                (map
                  #(is (is-member? test-tree %))
                  (range i)))
              (is (verify-rb-tree test-tree))
              (is (not (is-member? test-tree -10)))
              (is (not (is-member? test-tree (inc i))))
              )))
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
          #(is (is-member? tree %))
          coll))))
  (testing "size two"
    (let [coll (range 2)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree) (str tree))
      (doall
        (map
          #(is (is-member? tree %))
          coll))))
  (testing "size three"
    (let [coll (range 3)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree))
      (doall
        (map
          #(is (is-member? tree %))
          coll))))
  (testing "size four"
    (let [coll (range 4)
          tree (from-ord-list coll)]
      (is (verify-rb-tree tree) (str tree))
      (doall
        (map
          #(is (is-member? tree %))
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
                #(is (is-member? tree %))
                coll))))
      doall)))