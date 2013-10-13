(ns functional-data-structures.explicit-min-heap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.heap :refer :all ]
            [functional-data-structures.leftist-heap]
            [functional-data-structures.wb-leftist-heap :refer [weight-biased-leftist-heap]]
            [functional-data-structures.binomial-heap :refer [binomial-heap]]
            [functional-data-structures.explicit-min-heap :refer [explicit-min-heap]]))

(deftest test-find-min
  (testing "find min of single element heap"
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        (map
          #(insert % 1)
          heaps)
        (map
          #(is (= (find-min %) 1)))
        doall)))
  (testing "find min of 3 element heap"
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        (map
          #(-> %
             (insert 1)
             (insert 2)
             (insert 3))
          heaps)
        (map
          #(is (= (find-min %) 1)))
        doall))
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        (map
          #(-> %
             (insert 2)
             (insert 1)
             (insert 3))
          heaps)
        (map
          #(is (= (find-min %) 1)))
        doall)))
  (testing "exception"
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        heaps
        (map
          #(is (thrown? AssertionError (find-min %))))
        doall)))
  (testing "multiple inserts and find min"
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        (for [h heaps i (range 10)]
          [h (repeatedly 10 #(- 5000 (rand-int 10000)))])
        (map
          (fn [[h c]]
            (->> c
              (reduce
                insert
                h)
              (#(is (= (apply min c) (find-min %))))
              )))
        doall))))

(deftest test-delete-min
  (testing "exception"
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        heaps
        (map
          #(is (thrown? AssertionError (delete-min %))))
        doall)))
  (testing "multiple inserts and delete min"
    (let [heaps (map
                  explicit-min-heap
                  (list nil (weight-biased-leftist-heap) (binomial-heap)))]
      (->>
        (for [h heaps i (range 10)]
          [h (repeatedly 10 #(- 5000 (rand-int 10000)))])
        (map
          (fn [[h c]]
            (->> c
              (reduce
                insert
                h)
              (iterate delete-min)
              (take (count c))
              (map find-min)
              (#(is (= % (sort c)))))))
        doall))))
