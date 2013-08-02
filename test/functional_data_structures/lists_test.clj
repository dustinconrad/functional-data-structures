(ns functional-data-structures.lists-test
  (:require [clojure.test :refer :all ]
            [functional-data-structures.lists :refer :all ]))

(deftest test-append
  (testing "random appends"
    (->>
      (for [i (range 1 11)]
        [(take (rand-int 25) (repeatedly #(rand-int 25))) (take (rand-int 25) (repeatedly #(rand-int 25)))])
      (map
        (fn [[l r]]
          (is (= (concat l r) (++ l r)))))
      doall)))

(deftest test-update
  (testing "exception update"
    (is (thrown? IndexOutOfBoundsException (update [1] 3 5))))
  (testing "random updates"
    (->>
      (repeatedly 10 (fn [] (repeatedly (inc (rand-int 50)) #(rand-int 200))))
      (map vec)
      (map
        #(let [index (rand-int (count %))
               new (rand-int 200)]
           (is (= (update % index new) (assoc % index new)))))
      doall)))