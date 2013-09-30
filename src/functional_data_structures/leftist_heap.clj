(ns functional-data-structures.leftist-heap
  (:require [functional-data-structures.heap :refer :all ]
            [functional-data-structures.compare :refer :all ]))

(declare make-t)

;chapter 3.1
(defrecord LeftistHeap [rank value left right]
  Heap
  (is-empty? [_] false)
  (insert [h x]
    (merge-heap h (->LeftistHeap 1 x nil nil)))
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
    (->LeftistHeap 1 x nil nil))
  (merge-heap [_ h2]
    h2)
  (find-min [_] (throw (AssertionError. "Cannot find-min of an empty heap")))
  (delete-min [_] (throw (AssertionError. "Cannot delete-min of an empty heap"))))

(defn rank [{r :rank :as h}]
  (if (is-empty? h) 0 r))

(defn make-t [x a b]
  (if (>= (rank a) (rank b))
    (->LeftistHeap (inc (rank b)) x a b)
    (->LeftistHeap (inc (rank a)) x b a)))

; exercise 3.2
(defn smart-insert [{v :value l :left r :right :as h} x]
  (cond
    (is-empty? h) (->LeftistHeap 1 x nil nil)
    (lt? x v) (smart-insert (make-t x l r) v)
    (lt? (rank r) (rank l)) (make-t v l (smart-insert r x))
    :default (make-t v (smart-insert l x) r)))

;exercise 3.3
(defn from-seq [seq]
  (let [heaps (map #(->LeftistHeap 1 % nil nil) seq)]
    (loop [hs heaps]
      (if (= (count hs) 1)
        (first hs)
        (recur
          (map
            (fn [[l r]] (merge-heap l r))
            (partition 2 2 [nil] hs)))))))