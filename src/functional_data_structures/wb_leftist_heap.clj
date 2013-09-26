(ns functional-data-structures.wb-leftist-heap
  (:require [functional-data-structures.heap :refer :all ]
            [functional-data-structures.compare :refer :all ]))

(declare make-t)

(defrecord WeightBiasedLeftistHeap [weight value left right]
  Heap
  (is-empty? [_] false)
  (insert [h x]
    (merge-heap h (->WeightBiasedLeftistHeap 1 x nil nil)))
  (merge-heap [{x :value a1 :left b1 :right :as h1} {y :value a2 :left b2 :right :as h2}]
    (cond
      (is-empty? h2) h1
      (lte? x y) (make-t x a1 (merge-heap b1 h2))
      :else (make-t y a2 (merge-heap b2 h1))))
  (find-min [{x :value a :left b :right}]
    x)
  (delete-min [{x :value a :left b :right}]
    (merge-heap a b)))

(extend-type nil
  Heap
  (is-empty? [_] true)
  (insert [_ x]
    (->WeightBiasedLeftistHeap 1 x nil nil))
  (merge-heap [_ h2]
    h2)
  (find-min [_] (throw (AssertionError. "Cannot find-min of an empty heap")))
  (delete-min [_] (throw (AssertionError. "Cannot delete-min of an empty heap"))))

(defn weight [{w :weight :as h}]
  (if (is-empty? h) 0 w))

(defn make-t [x a b]
  (let [wa (weight a)
        wb (weight b)
        wt (+ 1 wa wb)]
    (if (gte? wa wb)
      (->WeightBiasedLeftistHeap wt x a b)
      (->WeightBiasedLeftistHeap wt x b a))))