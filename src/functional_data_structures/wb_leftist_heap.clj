(ns functional-data-structures.wb-leftist-heap
  (:require [functional-data-structures.heap :refer :all ]
            [functional-data-structures.compare :refer :all ]))

(declare make-t)

(defn get-weight [{w :weight :as h}]
  (if (is-empty? h) 0 w))

; exercise 3.4 and 3.4 c)
(defrecord WeightBiasedLeftistHeap [weight value left right]
  Heap
  (is-empty? [h] (= h (->WeightBiasedLeftistHeap 0 nil nil nil)))
  (insert [h x]
    (merge-heap h (->WeightBiasedLeftistHeap 1 x (->WeightBiasedLeftistHeap 0 nil nil nil) (->WeightBiasedLeftistHeap 0 nil nil nil))))
  (merge-heap [{x :value a1 :left b1 :right :as h1} {y :value a2 :left b2 :right :as h2}]
    (cond
      (is-empty? h1) h2
      (is-empty? h2) h1
      (lte? x y) (let [merged (merge-heap b1 h2)
                       wm (get-weight a1)
                       wb (get-weight merged)
                       wt (+ 1 wm wb)]
                   (if (gte? wm wb)
                     (->WeightBiasedLeftistHeap wt x a1 merged)
                     (->WeightBiasedLeftistHeap wt x merged a1)))
      :else (let [merged (merge-heap b2 h1)
                  wa (get-weight a2)
                  wm (get-weight merged)
                  wt (+ 1 wa wm)]
              (if (gte? wa wm)
                (->WeightBiasedLeftistHeap wt y a2 merged)
                (->WeightBiasedLeftistHeap wt y merged a2)))))
  (find-min [{x :value a :left b :right :as h}]
    (if (is-empty? h)
      (throw (AssertionError. "Cannot find-min of an empty heap"))
      x))
  (delete-min [{x :value a :left b :right :as h}]
    (if (is-empty? h)
      (throw (AssertionError. "Cannot delete-min of an empty heap"))
      (merge-heap a b))))

(defn make-t [x a b]
  (let [wa (get-weight a)
        wb (get-weight b)
        wt (+ 1 wa wb)]
    (if (gte? wa wb)
      (->WeightBiasedLeftistHeap wt x a b)
      (->WeightBiasedLeftistHeap wt x b a))))

; exercise 3.4 b)
(defn smart-insert [{v :value l :left r :right :as h} x]
  (cond
    (is-empty? h) (->WeightBiasedLeftistHeap 1 x (->WeightBiasedLeftistHeap 0 nil nil nil) (->WeightBiasedLeftistHeap 0 nil nil nil))
    (lt? x v) (smart-insert (make-t x l r) v)
    (lt? (get-weight r) (get-weight l)) (make-t v l (smart-insert r x))
    :default (make-t v (smart-insert l x) r)))

(defn weight-biased-leftist-heap
  ([] (->WeightBiasedLeftistHeap 0 nil nil nil))
  ([x] (->WeightBiasedLeftistHeap 1 x (weight-biased-leftist-heap) (weight-biased-leftist-heap))))
