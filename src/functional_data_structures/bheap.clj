(ns functional-data-structures.bheap)

(defn make-node [rank value children]
  {:rank rank :value value :children children})

(defn link [{r1 :rank x1 :value c1 :children :as t1} {r2 :rank x2 :value c2 :children :as t2}]
  {:pre [(= r1 r2)]}
  (if (<= x1 x2)
    (make-node (inc r1) x1 (cons t2 c1))
    (make-node (inc r1) x2 (cons t1 c2))))