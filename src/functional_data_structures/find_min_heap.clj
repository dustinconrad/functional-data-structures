(ns functional-data-structures.find-min-heap
  (:require [functional-data-structures.heap :refer :all ]))

(defrecord FindMinHeap [heap min]
  Heap
  (is-empty? [this]
    (is-empty? heap))
  
  )