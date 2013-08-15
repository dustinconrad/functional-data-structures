(ns functional-data-structures.lheap)

(defn rank [lheap]
  (if (empty? lheap)
    0
    (:rank lheap)))

(defn make-lheap
  ([value]
    (make-lheap 1 value nil nil))
  ([rank value left right]
    {:rank rank :value value :left left :right right})
  ([value a b]
    (if (>= (rank a) (rank b))
      (make-lheap (inc (rank b)) value a b)
      (make-lheap (inc (rank a)) value b a))))

(defn merge-lheap [h1 h2]
  (cond
    (empty? h1) h2
    (empty? h2) h1
    (<= (:value h1) (:value h2)) (make-lheap (:value h1) (:left h1) (merge-lheap (:right h1) h2))
    :default (make-lheap (:value h2) (:left h2) (merge-lheap (:right h2) h1))
    ))

(defn insert [lheap x]
  (merge-lheap lheap (make-lheap x)))

(defn smart-insert [lheap x]
  (cond
    (empty? lheap) (make-lheap x)
    (< x (:value lheap)) (smart-insert (make-lheap x (:left lheap) (:right lheap)) (:value lheap))
    (< (rank (:right lheap)) (rank (:left lheap))) (make-lheap (:value lheap) (:left lheap) (smart-insert (:right lheap) x))
    :default (make-lheap (:value lheap) (smart-insert (:left lheap) x) (:right lheap))
    )
  )

(defn find-min [lheap]
  {:pre [((complement empty?) lheap)]}
  (:value lheap))

(defn delete-min [lheap]
  {:pre [((complement empty?) lheap)]}
  (merge-lheap (:left lheap) (:right lheap)))