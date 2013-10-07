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

(defn insert-helper [ts x]
  (ins-tree ts (->BinomialTree 0 x nil)))

(defn merge-heap-helper [[t1 & ts1-prime :as ts1] [t2 & ts2-prime :as ts2]]
  (cond
    (empty? ts2) ts1
    (empty? ts1) ts2
    (< (rank t1) (rank t2)) (cons t1 (merge-heap-helper ts1-prime ts2))
    (< (rank t2) (rank t1)) (cons t2 (merge-heap-helper ts2-prime ts1))
    :default (ins-tree (merge-heap-helper ts1-prime ts2-prime) (link t1 t2))))

(defn remove-min-tree [[t & ts :as tee]]
  {:pre (not-empty tee)}
  (if (empty? ts)
    [t ts]
    (let [[t-prime ts-prime] (remove-min-tree ts)]
      (if (<= (:value t) (:value t-prime))
        [t ts]
        [t-prime (cons t ts-prime)]))))

(defrecord BinomialHeap [ts]
  Heap
  (is-empty? [this]
    (empty? ts))
  (insert [this x]
    (->BinomialHeap (insert-helper ts x)))
  (merge-heap [this {o :ts}]
    (->BinomialHeap (merge-heap-helper ts o)))
  (find-min [this]
    (let [[t _] (remove-min-tree ts)]
      (:value t)))
  (delete-min [this]
    (let [[{x :value ts1 :children} ts2] (remove-min-tree ts)]
      (merge-heap (reverse ts1) ts2))))

(defn binomial-heap
  ([] (->BinomialHeap nil))
  ([x] (->BinomialHeap x)))

(defn find-min-direct-helper [[{tv :value} & ts :as tee]]
  {:pre (not-empty tee)}
  (if (empty? ts)
    tv
    (let [tv-prime (find-min-direct-helper ts)]
      (if (lte? tv tv-prime)
        tv
        tv-prime))))

(defn find-min-direct [h]
  (find-min-direct-helper (:ts h)))