(ns functional-data-structures.heap)

(defprotocol Heap
  (is-empty? [h])
  (insert [h x])
  (merge-heap [hl hr])
  (find-min [h])
  (delete-min [h]))