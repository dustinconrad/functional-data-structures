(ns functional-data-structures.heap)

(defn make-heap
  ([rank value left right]
    {:rank rank :value value :left left :right right}))

