(ns functional-data-structures.wb-leftist-heap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.heap :refer :all ]
            [functional-data-structures.wb-leftist-heap :refer :all ]))

(defn make-wblheap
  ([value]
    (make-wblheap 1 value nil nil))
  ([rank value left right]
    (->WeightBiasedLeftistHeap rank value left right)))

(deftest test-make-lheap
  (testing "make a leftist heap with the auto-swapping based on rank logic"
    (let [two-level (make-t 9 (make-wblheap 10) (make-wblheap 11))
          three-level (make-t 0 (make-wblheap 1) two-level)]
      (is (= (:right three-level) (make-wblheap 1)))
      (is (= (:left three-level) two-level)))))

(deftest test-insert
  (testing "insert into empty leftist heap"
    (let [expected (make-wblheap 1)
          actual (insert (weight-biased-leftist-heap) 1)]
      (is (= actual expected))))
  (testing "insert into rank 1 leftist heap"
    (let [one-level (make-wblheap 1)
          expected (make-t 1 (make-wblheap 2) nil)
          actual (insert one-level 2)]
      (is (= actual expected)))
    (let [one-level (make-wblheap 1)
          expected (make-t 0 (make-wblheap 1) nil)
          actual (insert one-level 0)]
      (is (= actual expected)))
    )
  )

(deftest test-find-min
  (testing "find min of rank 1 leftist heap"
    (let [lheap (insert (weight-biased-leftist-heap) 1)]
      (is (= (find-min lheap) 1))))
  (testing "find min of leftist heap"
    (let [lheap (-> (weight-biased-leftist-heap) (insert 1) (insert 2))]
      (is (= (find-min lheap) 1)))
    (let [lheap (-> (weight-biased-leftist-heap) (insert 1) (insert 0))]
      (is (= (find-min lheap) 0))))
  (testing "exception"
    (is (thrown? AssertionError (find-min (weight-biased-leftist-heap)))))
  (testing "multiple inserts and find min"
    (->>
      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
      (map
        (fn [c]
          (->> c
            (reduce
              insert
              (weight-biased-leftist-heap))
            (#(is (= (apply min c) (find-min %))))
            )))
      doall)))

(deftest test-delete-min
  (testing "delete min of rank 1 leftist heap"
    (let [lheap (insert (weight-biased-leftist-heap) 1)
          actual (delete-min lheap)
          expected nil]
      (is (= actual expected))))
  (testing "delete min of leftist heap"
    (let [lheap (-> (weight-biased-leftist-heap) (insert 1) (insert 2))
          old-min (find-min lheap)]
      (is (not= old-min (find-min (delete-min lheap)))))
    (let [lheap (-> (weight-biased-leftist-heap) (insert 1) (insert 0))
          old-min (find-min lheap)]
      (is (not= old-min (find-min (delete-min lheap))))))
  (testing "exception"
    (is (thrown? AssertionError (delete-min nil))))
  (testing "multiple inserts and find/delete min"
    (->>
      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
      (map
        (fn [c]
          (->> c
            (reduce
              insert
              (weight-biased-leftist-heap))
            (iterate delete-min)
            (take (count c))
            (map find-min)
            (#(is (= % (sort c)))))))
      doall)))