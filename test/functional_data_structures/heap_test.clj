(ns functional-data-structures.heap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.heap :refer :all ]))

(deftest test-make-heap
  (testing "make a heap with the full constructor"
    (let [expected {:rank 0 :value "value" :left "left" :right "right"}
          actual (make-heap 0 "value" "left" "right")]
      (is (= actual expected))))
  (testing "make a heap with the shorthand constructor"
    (let [expected {:rank 1 :value "value" :left nil :right nil}
          actual (make-heap "value")]
      (is (= actual expected))))
  (testing "make a heap with the auto-swapping based on rank logic"
    (let [two-level (make-heap 9 (make-heap 10) (make-heap 11))
          three-level (make-heap 0 (make-heap 1) two-level)]
      (is (= (:right three-level) (make-heap 1)))
      (is (= (:left three-level) two-level))
      )))

(deftest test-insert
  (testing "insert into empty heap"
    (let [expected (make-heap 1)
          actual (insert nil 1)]
      (is (= actual expected))))
  (testing "insert into rank 1 heap"
    (let [one-level (make-heap 1)
          expected (make-heap 1 (make-heap 2) nil)
          actual (insert one-level 2)]
      (is (= actual expected)))
    (let [one-level (make-heap 1)
          expected (make-heap 0 (make-heap 1) nil)
          actual (insert one-level 0)]
      (is (= actual expected)))
    )
  )

(deftest test-find-min
  (testing "find min of rank 1 heap"
    (let [heap (insert nil 1)]
      (is (= (find-min heap) 1))))
  (testing "find min of heap"
    (let [heap (-> nil (insert 1) (insert 2))]
      (is (= (find-min heap) 1)))
    (let [heap (-> nil (insert 1) (insert 0))]
      (is (= (find-min heap) 0)))
    )
  (testing "exception"
    (is (thrown? AssertionError (find-min nil))))
  (testing "multiple inserts and find min"
    (->>
      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
      (map
        (fn [c]
          (->> c
            (reduce
              insert
              nil)
            (#(is (= (apply min c) (find-min %))))
            )))
      doall))
  )

(deftest test-delete-min
  (testing "delete min of rank 1 heap"
    (let [heap (insert nil 1)
          actual (delete-min heap)
          expected nil]
      (is (= actual expected))))
  (testing "delete min of heap"
    (let [heap (-> nil (insert 1) (insert 2))
          old-min (find-min heap)]
      (is (not= old-min (find-min (delete-min heap)))))
    (let [heap (-> nil (insert 1) (insert 0))
          old-min (find-min heap)]
      (is (not= old-min (find-min (delete-min heap)))))
    )
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
            (#(is (not= (apply min c) (find-min (delete-min %)))))
            )))
      doall))
  )