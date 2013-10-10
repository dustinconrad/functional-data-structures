(ns functional-data-structures.binomial-heap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.binomial-heap :refer :all ]
            [functional-data-structures.heap :refer :all ]))

(deftest test-link
  (testing "test rank 0"
    (let [first (binomial-tree 5)
          second (binomial-tree 4)
          expected (binomial-tree 1 4 (list (->Tree 5 nil)))]
      (is (= (link first second) expected))
      (is (= (link second first) expected))))
  (testing "exception "
    (let [first (binomial-tree 5)
          second (binomial-tree 4)
          expected (binomial-tree 1 4 (list (binomial-tree 5)))]
    (is (thrown? AssertionError (link expected first)))
    (is (thrown? AssertionError (link first expected)))))
  (testing "test rank 1"
    (let [first (binomial-tree 1 1 (list (->Tree 3 nil)))
          second (binomial-tree 1 5 (list (->Tree 7 nil)))
          expected (binomial-tree 2 1
                     (list
                       (->Tree 5 (list (->Tree 7 nil)))
                       (->Tree 3 nil)))]
      (is (= (link first second) expected))
      (is (= (link second first) expected)))))

(deftest test-insert
  (testing "insert into empty"
    (is (= (binomial-heap (list (binomial-tree 1))) (insert (binomial-heap) 1))))
  (testing "insert into front"
    (let [one (insert (binomial-heap) 10)
          two (insert one 20)
          inserted (insert two 1)]
      (is (= (binomial-tree 1) (first (:ts inserted))))
      (is (not-empty (rest (:ts inserted))))))
  (testing "insert into middle"
    (let [one (insert (binomial-heap) 10)
          two (insert one 20)
          one-two (insert two 5)
          three (insert one-two 1)]
      (is (empty? (rest three)))
      (is ((complement nil?) (first three))))))

(deftest test-merge
  (testing "empty merge"
    (let [one (binomial-heap (list 1))]
      (is (= one (merge-heap (binomial-heap) one)))
      (is (= one (merge-heap one (binomial-heap))))
      (is (= one (merge-heap (binomial-heap) one)))))
  (testing "merge rank 0 and rank 1"
    (let [one (insert (binomial-heap) 10)
          two (insert one 20)
          one-two (insert two 10)]
      (is (= one-two (merge-heap one two)))
      (is (= one-two (merge-heap two one)))))
  (testing "merge same rank"
    (let [one (insert (binomial-heap) 10)
          uno (insert (binomial-heap) 20)
          expected (insert one 20)]
      (is (= expected (merge-heap one uno)))
      (is (= expected (merge-heap uno one)))))
  (testing "merge size two"
    (let [one (-> (binomial-heap) (insert 10) (insert 20) (insert 30))
          uno (-> (binomial-heap) (insert 35) (insert 25) (insert 15))
          expected  (-> (binomial-heap) (insert 10) (insert 20) (insert 25) (insert 35) (insert 15) (insert 30))]
      (is (= expected (merge-heap one uno))))))

(deftest test-find-min
  (testing "find min of size 1 binomial heap"
    (let [bheap (insert (binomial-heap) 1)]
      (is (= (find-min bheap) 1))))
  (testing "find min of binomial heap"
    (let [bheap (-> (binomial-heap) (insert 1) (insert 2))]
      (is (= (find-min bheap) 1)))
    (let [bheap (-> (binomial-heap) (insert 1) (insert 0))]
      (is (= (find-min bheap) 0))))
  (testing "exception"
    (is (thrown? AssertionError (find-min (binomial-heap)))))
  (testing "multiple inserts and find min"
    (->>
      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
      (map
        (fn [c]
          (->> c
            (reduce
              insert
              (binomial-heap))
            (#(is (= (apply min c) (find-min %)))))))
      doall)))

(deftest test-find-direct
  (testing "find min direct of size 1 binomial heap"
    (let [bheap (insert (binomial-heap) 1)]
      (is (= (find-min-direct bheap) 1))))
  (testing "find min direct of binomial heap"
    (let [bheap (-> (binomial-heap) (insert 1) (insert 2))]
      (is (= (find-min-direct bheap) 1)))
    (let [bheap (-> (binomial-heap) (insert 1) (insert 0))]
      (is (= (find-min-direct bheap) 0))))
  (testing "exception"
    (is (thrown? AssertionError (find-min-direct (binomial-heap)))))
  (testing "multiple inserts and find min direct"
    (->>
      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
      (map
        (fn [c]
          (->> c
            (reduce
              insert
              (binomial-heap))
            (#(is (= (apply min c) (find-min-direct %))))
            )))
      doall)))

(deftest test-delete-min
  (testing "delete min of size 1 binomial heap"
    (let [bheap (insert nil 1)
          actual (delete-min bheap)]
      (is (empty? actual))))
  (testing "delete min of leftist heap"
    (let [bheap (-> nil (insert 1) (insert 2))
          old-min (find-min bheap)]
      (is (not= old-min (find-min (delete-min bheap)))))
    (let [bheap (-> nil (insert 1) (insert 0))
          old-min (find-min bheap)]
      (is (not= old-min (find-min (delete-min bheap))))))
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
              nil)
            (iterate delete-min)
            (take (count c))
            (map find-min)
            (#(is (= % (sort c))))
            )))
      doall)))