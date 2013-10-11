(ns functional-data-structures.explicit-min-heap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.explicit-min-heap :refer :all ]
            [functional-data-structures.heap :refer :all ]))

(deftest test-insert
  (testing "insert into empty leftist heap"
    (let [expected (make-lheap 1)
          actual (insert nil 1)]
      (is (= actual expected))))
  (testing "insert into rank 1 leftist heap"
    (let [one-level (make-lheap 1)
          expected (make-t 1 (make-lheap 2) nil)
          actual (insert one-level 2)]
      (is (= actual expected)))
    (let [one-level (make-lheap 1)
          expected (make-t 0 (make-lheap 1) nil)
          actual (insert one-level 0)]
      (is (= actual expected)))))

;(deftest test-find-min
;  (testing "find min of rank 1 leftist heap"
;    (let [lheap (insert nil 1)]
;      (is (= (find-min lheap) 1))))
;  (testing "find min of leftist heap"
;    (let [lheap (-> nil (insert 1) (insert 2))]
;      (is (= (find-min lheap) 1)))
;    (let [lheap (-> nil (insert 1) (insert 0))]
;      (is (= (find-min lheap) 0))))
;  (testing "exception"
;    (is (thrown? AssertionError (find-min nil))))
;  (testing "multiple inserts and find min"
;    (->>
;      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
;      (map
;        (fn [c]
;          (->> c
;            (reduce
;              insert
;              nil)
;            (#(is (= (apply min c) (find-min %))))
;            )))
;      doall)))
;
;(deftest test-delete-min
;  (testing "delete min of rank 1 leftist heap"
;    (let [lheap (insert nil 1)
;          actual (delete-min lheap)
;          expected nil]
;      (is (= actual expected))))
;  (testing "delete min of leftist heap"
;    (let [lheap (-> nil (insert 1) (insert 2))
;          old-min (find-min lheap)]
;      (is (not= old-min (find-min (delete-min lheap)))))
;    (let [lheap (-> nil (insert 1) (insert 0))
;          old-min (find-min lheap)]
;      (is (not= old-min (find-min (delete-min lheap))))))
;  (testing "exception"
;    (is (thrown? AssertionError (delete-min nil))))
;  (testing "multiple inserts and find/delete min"
;    (->>
;      (repeatedly 10 (fn [] (repeatedly 10 #(- 5000 (rand-int 10000)))))
;      (map
;        (fn [c]
;          (->> c
;            (reduce
;              insert
;              nil)
;            (iterate delete-min)
;            (take (count c))
;            (map find-min)
;            (#(is (= % (sort c)))))))
;      doall)))
