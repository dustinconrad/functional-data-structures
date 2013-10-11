(ns functional-data-structures.explicit-min-heap
  (:require [functional-data-structures.heap :refer :all ]))

(defrecord ExplicitMinHeap [heap min]
  Heap
  (is-empty? [this]
    (is-empty? heap))
  (insert [this x]
    (->ExplicitMinHeap
      (insert heap x)
      (cond
        (nil? min) x
        (lt? x min) x
        :else min)))
  (merge-heap [this other]
    (let [merged (merge-heap heap other)]
      (->ExplicitMinHeap
        merged
        (find-min merged))))
  (find-min [this]
    (if (is-empty? heap)
      (throw (AssertionError. "Cannot find-min of an empty heap"))
      min))
  (delete-min [this]
    (if (is-empty? heap)
      (throw (AssertionError. "Cannot delete-min of an empty heap"))
      (let [deleted (delete-min heap)]
        (->ExplicitMinHeap
          deleted
          (try
            (find-min deleted)
            (catch AssertionError ae nil)))))))

(defn explicit-min-heap [h]
  (->ExplicitMinHeap h nil))