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