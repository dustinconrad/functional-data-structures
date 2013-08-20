(ns functional-data-structures.lheap)

(defn rank [{r :rank :as lheap}]
  (if (empty? lheap) 0 r))

(defn make-lheap
  ([value]
    (make-lheap 1 value nil nil))
  ([rank value left right]
    {:rank rank :value value :left left :right right})
  ([value a b]
    (if (>= (rank a) (rank b))
      (make-lheap (inc (rank b)) value a b)
      (make-lheap (inc (rank a)) value b a))))

(defn merge-lheap [{x :value a1 :left b1 :right :as h1} {y :value a2 :left b2 :right :as h2}]
  (cond
    (empty? h1) h2
    (empty? h2) h1
    (<= x y) (make-lheap x a1 (merge-lheap b1 h2))
    :default (make-lheap y a2 (merge-lheap b2 h1))
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

(defn lheap-from-seq [seq]
  (let [heaps (map make-lheap seq)]
    (loop [hs heaps]
      (if (= (count hs) 1)
        (first hs)
        (recur
          (map
            (fn [[l r]] (merge-lheap l r))
            (partition 2 2 [nil] hs)))))))