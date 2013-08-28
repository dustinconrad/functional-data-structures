(ns functional-data-structures.bheap-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.bheap :refer :all ]))

(deftest test-link
  (testing "test rank 0"
    (let [first (make-node 5)
          second (make-node 4)
          expected {:rank 1 :value 4 :children '({:rank 0 :value 5 :children nil})}]
      (is (= (link first second) expected))
      (is (= (link second first) expected))))
  (testing "exception "
    (let [first (make-node 5)
          second (make-node 4)
          expected {:rank 1 :value 4 :children '({:rank 0 :value 5 :children nil})}]
    (is (thrown? AssertionError (link expected first)))
    (is (thrown? AssertionError (link first expected)))))
  (testing "test rank 1"
    (let [first {:rank 1 :value 1 :children '({:rank 0 :value 3 :children nil})}
          second {:rank 1 :value 5 :children '({:rank 0 :value 7 :children nil})}
          expected {:rank 2 :value 1 :children 
                    '( {:rank 1 :value 5 :children ({:rank 0 :value 7 :children nil})}
                       {:rank 0 :value 3 :children nil})}]
      (is (= (link first second) expected))
      (is (= (link second first) expected))))
   )

(deftest test-insert
  (testing "insert into empty"
    (is (= (list (make-node 1)) (insert nil 1)))
    (is (= (list (make-node 1)) (insert '() 1))))
  (testing "insert into front"
    (let [one (insert nil 10)
          two (insert one 20)
          inserted (insert two 1)]
      (is (= (make-node 1) (first inserted)))
      (is (not-empty (rest inserted)))
      ))
  (testing "insert into middle"
    (let [one (insert nil 10)
          two (insert one 20)
          one-two (insert two 5)
          three (insert one-two 1)]
      (is (empty? (rest three)))
      (is ((complement nil?) (first three)))
      )))

(deftest test-merge
  (testing "empty merge"
    (let [one (list (make-node 1))]
      (is (= one (merge-bheap nil one)))
      (is (= one (merge-bheap one nil)))
      (is (= one (merge-bheap '() one)))
      (is (= one (merge-bheap one '())))))
  (testing "merge rank 0 and rank 1"
    (let [one (insert nil 10)
          two (insert one 20)
          one-two (insert two 10)]
      (is (= one-two (merge-bheap one two)))
      (is (= one-two (merge-bheap two one)))))
  (testing "merge same rank"
    (let [one (insert nil 10)
          uno (insert nil 20)
          expected (insert one 20)]
      (is (= expected (merge-bheap one uno)))
      (is (= expected (merge-bheap uno one)))))
  (testing "merge size two"
    (let [one (-> nil (insert 10) (insert 20) (insert 30))
          uno (-> nil (insert 35) (insert 25) (insert 15))
          expected  (-> nil (insert 10) (insert 20) (insert 25) (insert 35) (insert 15) (insert 30))]
      (is (= expected (merge-bheap one uno))))))


