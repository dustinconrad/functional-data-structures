(ns functional-data-structures.wb-leftist-heap
  (:require [functional-data-structures.heap :refer :all ]
            [functional-data-structures.compare :refer :all ]))

(declare make-t)

(defrecord WeightBiasedLeftistHeap [weight value left right]
  Heap
  (is-empty? [h] (= h (->WeightBiasedLeftistHeap 0 nil nil nil)))
  (insert [h x]
    (merge-heap h (->WeightBiasedLeftistHeap 1 x nil nil)))
  (merge-heap [{x :value a1 :left b1 :right :as h1} {y :value a2 :left b2 :right :as h2}]
    (cond
      (is-empty? h1) h2
      (is-empty? h2) h1
      (lte? x y) (make-t x a1 (merge-heap b1 h2))
      :else (make-t y a2 (merge-heap b2 h1))))
  (find-min [{x :value a :left b :right :as h}]
    (if (is-empty? h)
      (throw (AssertionError. "Cannot find-min of an empty heap"))
      x))
  (delete-min [{x :value a :left b :right :as h}]
    (if (is-empty? h)
      (throw (AssertionError. "Cannot find-min of an empty heap"))
      (merge-heap a b))))

(defn weight [{w :weight :as h}]
  (if (is-empty? h) 0 w))

(defn make-t [x a b]
  (let [wa (weight a)
        wb (weight b)
        wt (+ 1 wa wb)]
    (if (gte? wa wb)
      (->WeightBiasedLeftistHeap wt x a b)
      (->WeightBiasedLeftistHeap wt x b a))))

(defn weight-biased-leftist-heap
  ([] (->WeightBiasedLeftistHeap 0 nil nil nil))
  ([x] (->WeightBiasedLeftistHeap 1 x nil nil)))
