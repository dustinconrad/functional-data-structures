(ns functional-data-structures.binomial-heap
  (:require [functional-data-structures.heap :refer :all ]
            [functional-data-structures.compare :refer :all ]))

(defrecord BinomialTree [rank value children])

(defn link [{r1 :rank x1 :value c1 :children :as t1} {r2 :rank x2 :value c2 :children :as t2}]
  {:pre [(eq? r1 r2)]}
  (if (lte? x1 x2)
    (->BinomialTree (inc r1) x1 (cons t2 c1))
    (->BinomialTree (inc r1) x2 (cons t1 c2))))

(defn binomial-tree
  ([value]
    (binomial-tree 0 value nil))
  ([rank value children]
    (->BinomialTree rank value children)))

(defn rank [{r :rank x :value c :children}]
  r)

(defn ins-tree [[t-prime & ts-prime :as ts] t]
  (cond
    (empty? ts) (list t)
    (< (rank t) (rank t-prime)) (cons t ts)
    :else (ins-tree ts-prime (link t t-prime))))

(defrecord BinomialHeap [tree-list]
  Heap
  (is-empty? [h]
    (empty? tree-list))
  (insert [h x]
    )
  (merge-heap [hl hr])
  (find-min [h])
  (delete-min [h]))

(defn insert [ts x]
  (ins-tree ts (binomial-tree x)))

(defn merge-bheap [[t1 & ts1-prime :as ts1] [t2 & ts2-prime :as ts2]]
  (cond 
    (empty? ts2) ts1
    (empty? ts1) ts2
    (< (rank t1) (rank t2)) (cons t1 (merge-bheap ts1-prime ts2))
    (< (rank t2) (rank t1)) (cons t2 (merge-bheap ts2-prime ts1))
    :default (ins-tree (merge-bheap ts1-prime ts2-prime) (link t1 t2))
    ))

(defn remove-min-tree [[t & ts :as tee]]
  {:pre ((complement empty?) tee)}
  (if (empty? ts)
    [t ts]
    (let [[t-prime ts-prime] (remove-min-tree ts)]
      (if (<= (:value t) (:value t-prime))
        [t ts]
        [t-prime (cons t ts-prime)]))))

(defn find-min [ts]
  (let [[t _] (remove-min-tree ts)]
    (:value t)))

(defn find-min-direct [[{tv :value :as t} & ts :as tee]]
  {:pre ((complement empty?) tee)}
  (if (empty? ts)
    tv
    (let [tv-prime (find-min-direct ts)]
      (if (<= tv tv-prime)
        tv
        tv-prime))))

(defn delete-min [ts]
  (let [[{x :value ts1 :children} ts2] (remove-min-tree ts)]
    (merge-bheap (reverse ts1) ts2)))