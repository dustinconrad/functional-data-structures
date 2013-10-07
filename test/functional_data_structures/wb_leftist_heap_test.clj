(ns functional-data-structures.wb-leftist-heap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.heap :refer :all ]
            [functional-data-structures.wb-leftist-heap :refer :all ]))

(deftest test-make-lheap
  (testing "make a weight biased leftist heap with the auto-swapping based on rank logic"
    (let [two-level (make-t 9 (weight-biased-leftist-heap 10) (weight-biased-leftist-heap 11))
          three-level (make-t 0 (weight-biased-leftist-heap 1) two-level)]
      (is (= (:right three-level) (weight-biased-leftist-heap 1)))
      (is (= (:left three-level) two-level)))))

(deftest test-insert
  (testing "insert into empty weight biased leftist heap"
    (let [expected (weight-biased-leftist-heap 1)
          actual (insert (weight-biased-leftist-heap) 1)]
      (is (= actual expected))))
  (testing "insert into weight 1 weight biased leftist heap"
    (let [one-level (weight-biased-leftist-heap 1)
          expected (make-t 1 (weight-biased-leftist-heap 2) (weight-biased-leftist-heap))
          actual (insert one-level 2)]
      (is (= actual expected)))
    (let [one-level (weight-biased-leftist-heap 1)
          expected (make-t 0 (weight-biased-leftist-heap 1) (weight-biased-leftist-heap))
          actual (insert one-level 0)]
      (is (= actual expected)))))

(deftest test-find-min
  (testing "find min of weight 1 weight biased leftist heap"
    (let [lheap (insert (weight-biased-leftist-heap) 1)]
      (is (= (find-min lheap) 1))))
  (testing "find min of weight biased leftist heap"
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
  (testing "delete min of weight 1 weight biased leftist heap"
    (let [lheap (insert (weight-biased-leftist-heap) 1)
          actual (delete-min lheap)
          expected (weight-biased-leftist-heap)]
      (is (= actual expected))))
  (testing "delete min of weight biased leftist heap"
    (let [lheap (-> (weight-biased-leftist-heap) (insert 1) (insert 2))
          old-min (find-min lheap)]
      (is (not= old-min (find-min (delete-min lheap)))))
    (let [lheap (-> (weight-biased-leftist-heap) (insert 1) (insert 0))
          old-min (find-min lheap)]
      (is (not= old-min (find-min (delete-min lheap))))))
  (testing "exception"
    (is (thrown? AssertionError (delete-min (weight-biased-leftist-heap)))))
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

(deftest test-smart-insert
  (testing "insert into empty weight biased leftist heap"
    (let [expected (weight-biased-leftist-heap 1)
          actual (smart-insert (weight-biased-leftist-heap) 1)]
      (is (= actual expected))))
  (testing "insert into weight 1 weight biased leftist heap"
    (let [one-level (weight-biased-leftist-heap 1)
          expected (make-t 1 (weight-biased-leftist-heap 2) (weight-biased-leftist-heap))
          actual (smart-insert one-level 2)]
      (is (= actual expected)))
    (let [one-level (weight-biased-leftist-heap 1)
          expected (make-t 0 (weight-biased-leftist-heap 1) (weight-biased-leftist-heap))
          actual (smart-insert one-level 0)]
      (is (= actual expected)))))

(deftest test-smart-insert-find-min
  (testing "find min of weight 1 weight biased leftist heap"
    (let [lheap (smart-insert (weight-biased-leftist-heap) 1)]
      (is (= (find-min lheap) 1))))
  (testing "find min of weight biased leftist heap"
    (let [lheap (-> (weight-biased-leftist-heap) (smart-insert 1) (smart-insert 2))]
      (is (= (find-min lheap) 1)))
    (let [lheap (-> (weight-biased-leftist-heap) (smart-insert 1) (smart-insert 0))]
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
              smart-insert
              (weight-biased-leftist-heap))
            (#(is (= (apply min c) (find-min %))))
            )))
      doall)))