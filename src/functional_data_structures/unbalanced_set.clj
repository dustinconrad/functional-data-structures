(ns functional-data-structures.unbalanced-set
  (:require [functional-data-structures.set :refer :all ])
  (:require [functional-data-structures.compare :refer :all ]))

;chapter 2.2
(defrecord UnbalancedSet [left value right]
  Set
  (is-empty? [_] false)
  (is-member? [{a :left y :value b :right} x]
    (cond
      (lt? x y) (is-member? a x)
      (gt? x y) (is-member? b x)
      :equal true))
  (insert [{a :left y :value b :right :as this} x]
    (cond
      (lt? x y) (->UnbalancedSet (insert a x) y b)
      (gt? x y) (->UnbalancedSet a y (insert b x))
      :equal this)))

(extend-type nil
  Set
  (is-empty? [_] true)
  (is-member? [_ _] false)
  (insert [_ x]
    (->UnbalancedSet nil x nil)))

;exercise 2.2
(defn- smart-member-helper [{a :left y :value b :right :as tree} x max]
  (cond
    (is-empty? tree) (eq? x max)
    (lte? x y) (recur a x y)
    :default (recur b x max)))

(defn smart-member? [s x]
  (smart-member-helper s x nil))

;exercise 2.3 and 2.4
(defn- smart-insert-helper [{a :left y :value b :right :as tree} x max]
  (cond
    (is-empty? tree) (if (= x max) (throw (IllegalArgumentException. (str x " is already in the set"))) (insert tree x))
    (lte? x y) (->UnbalancedSet (smart-insert-helper a x y) y b)
    :default (->UnbalancedSet a y (smart-insert-helper b x max))))

(defn smart-insert [s x]
  (try
    (smart-insert-helper s x nil)
    (catch IllegalArgumentException iae s)))

;exercise 2.5
;a)
(defn complete [x d]
  (if (zero? d)
    (->UnbalancedSet nil x nil)
    (let [node (complete x (dec d))]
      (->UnbalancedSet node x node))))
;b)
(defn- create-helper [min max]
  (cond
    (>= min max) nil
    :default (let [mid (quot (+ min max) 2)]
               (->UnbalancedSet
                 (create-helper min mid)
                 mid
                 (create-helper (inc mid) max)))))

(defn create [n]
  (create-helper 0 n))

;exercise 2.6
(defrecord FiniteMap [left key value right]
  Map
  (is-empty-map? [_] false)
  (bind [{a :left y :key z :value b :right :as this} k v]
    (cond
      (lt? k y) (->FiniteMap (bind a k v) y z b)
      (gt? k y) (->FiniteMap a y z (bind b k v))
      :equal (->FiniteMap a k v b)))
  (lookup [{a :left y :key z :value b :right :as this} k]
    (cond
      (lt? k y) (lookup a k)
      (gt? k y) (lookup b k)
      :equal z)))

(extend-protocol Map
  nil
  (is-empty-map? [_] true)
  (bind [_ k v] (->FiniteMap nil k v nil))
  (lookup [_ _] (throw (Exception. "NotFound"))))
