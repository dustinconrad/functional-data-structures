(ns functional-data-structures.stream-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.stream :refer [++ insertion-sort]]))

(defmacro is-lazy? [x] `(is (instance? clojure.lang.LazySeq ~x)))

(deftest test-append
  (testing "random appends"
    (->>
      (for [i (range 1 11)]
        [(take (inc (rand-int 24)) (repeatedly #(rand-int 25))) (take (inc (rand-int 24)) (repeatedly #(rand-int 25)))])
      (map
        (fn [[left right]]
          (let [l (doall left)
                r (doall right)
                together (++ l r)]
            (is (realized? l) (str left right (concat left right)))
            (is (realized? r) (str left right (concat left right)))
            (is (is-lazy? together) (str left right (concat left right)))
            (is (not (realized? together)) (str left right (concat left right)))
            (is (= (concat l r) (++ l r)) (str left right (concat left right))))))
      doall)))

(deftest test-take
  (testing "random takes"
    (->>
      (for [i (range 1 11)]
        [(inc (rand-int 24)) (take (inc (rand-int 24)) (repeatedly #(rand-int 25)))])
      (map
        (fn [[n c]]
          (let [coll (doall c)
                taken (functional-data-structures.stream/take n coll)]
            (is (realized? c))
            (is (is-lazy? taken))
            (is (not (realized? taken)))
            (is (= (take n coll) taken)))))
      doall)))

(deftest test-drop
  (testing "random drops"
    (->>
      (for [i (range 1 11)]
        [(inc (rand-int 24)) (take (inc (rand-int 24)) (repeatedly #(rand-int 25)))])
      (map
        (fn [[n c]]
          (let [coll (doall c)
                dropped (functional-data-structures.stream/drop n coll)]
            (is (realized? c))
            (is (is-lazy? dropped))
            (is (not (realized? dropped)))
            (is (= (drop n coll) dropped)))))
      doall)))

(deftest test-reverse
  (testing "random reverses"
    (->>
      (for [i (range 1 11)]
        (take (inc (rand-int 25)) (repeatedly #(rand-int 25))))
      (map
        (fn [c]
          (let [coll (doall c)
                reversed (functional-data-structures.stream/reverse coll)]
            (is (realized? c))
            (is (is-lazy? reversed))
            (is (not (realized? reversed)))
            (is (= (reverse coll) reversed)))))
      doall)))

(deftest test-insertion-sort
  (testing "random appends"
    (->>
      (for [i (range 1 11)]
        [(inc (rand-int 24)) (take (rand-int 25) (repeatedly #(rand-int 100)))])
      (map
        (fn [[k coll]]
          (let [ins-sorted (insertion-sort coll k)
                sorted (sort coll)]
            (is (= (take k sorted) (take k ins-sorted))))))
      doall)))
