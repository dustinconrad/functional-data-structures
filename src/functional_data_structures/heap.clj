(ns functional-data-structures.heap)

(defn rank [heap]
  (if (empty? heap)
    0
    (:rank heap)))

(defn make-heap
  ([value]
    (make-heap 1 value nil nil))
  ([rank value left right]
    {:rank rank :value value :left left :right right})
  ([value a b]
    (if (>= (rank a) (rank b))
      (make-heap (inc (rank b)) value a b)
      (make-heap (inc (rank a)) value b a))))

(defn merge-heap [h1 h2]
  (cond
    (empty? h1) h2
    (empty? h2) h2
    (<= (:value h1) (:value h2)) (make-heap (:value h1) (:left h1) (merge-heap (:right h1) h2))
    :default (make-heap (:value h2) (:left h2) (merge-heap (:right h2) h1))
    ))

