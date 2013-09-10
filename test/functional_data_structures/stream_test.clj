(ns functional-data-structures.stream-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.stream :refer :all :exclude [take]]))

(defmacro is-lazy? [x] `(is (instance? clojure.lang.LazySeq ~x)))

(deftest test-append
  (testing "random appends"
    (->>
      (for [i (range 1 11)]
        [(take (rand-int 25) (repeatedly #(rand-int 25))) (take (rand-int 25) (repeatedly #(rand-int 25)))])
      (map
        (fn [[left right]]
          (let [l (doall left)
                r (doall right)
                together (++ l r)]
            (is (realized? l)) 
            (is (realized? r))
            (is (is-lazy? together))
            (is (not (realized? together)))
            (is (= (concat l r) (++ l r))))))
      doall)))

(deftest test-take
  (testing "random takes"
    (->>
      (for [i (range 1 11)]
        [(rand-int 25) (take (rand-int 25) (repeatedly #(rand-int 25)))])
      (map
        (fn [[n c]]
          (let [coll (doall c)
                taken (functional-data-structures.stream/take n coll)]
            (is (realized? c))
            (is (is-lazy? taken))
            (is (not (realized? taken)))
            (is (= (take n coll) taken)))))
      doall)))