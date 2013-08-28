(ns functional-data-structures.bheap)

(defn make-node 
  ([value]
    (make-node 0 value nil))
  ([rank value children]
    {:rank rank :value value :children children}))

(defn link [{r1 :rank x1 :value c1 :children :as t1} {r2 :rank x2 :value c2 :children :as t2}]
  {:pre [(= r1 r2)]}
  (if (<= x1 x2)
    (make-node (inc r1) x1 (cons t2 c1))
    (make-node (inc r1) x2 (cons t1 c2))))

(defn rank [{r :rank x :value c :children}]
  r)

(defn ins-tree [[t-prime & ts-prime :as ts] t]
  (cond 
    (empty? ts) (list t)
    (< (rank t) (rank t-prime)) (cons t ts)
    :else (ins-tree ts-prime (link t t-prime))
    ))

(defn insert [ts x]
  (ins-tree ts (make-node x)))

(defn merge-bheap [[t1 & ts1-prime :as ts1] [t2 & ts2-prime :as ts2]]
  (cond 
    (empty? ts2) ts1
    (empty? ts1) ts2
    (< (rank t1) (rank t2)) (cons t1 (merge-bheap ts1-prime ts2))
    (< (rank t2) (rank t1)) (cons t2 (merge-bheap ts2-prime ts1))
    :default (ins-tree (merge-bheap ts1-prime ts2-prime) (link t1 t2))
    ))