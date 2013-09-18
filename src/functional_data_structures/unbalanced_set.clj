(ns functional-data-structures.unbalanced-set
  (:require [functional-data-structures.set :refer :all]))

(defn- lt? [x y]
  (neg? (compare x y)))

(defn- gt? [x y]
  (pos? (compare x y)))

(defn- lte? [x y]
  (not (gt? x y)))

(defn- eq? [x y]
  (zero? (compare x y)))

(defn- smart-member-helper [{a :left y :value b :right :as tree} x max]
  (cond
    (is-empty? tree) (eq? x max)
    (lte? x y) (recur a x y)
    :default (recur b x max)))

(declare smart-insert-helper)

(defrecord UnbalancedSet [left value right]
  Set
  (is-empty? [_] false)
  (is-member? [{a :left y :value b :right :as this} x]
    (cond
      (lt? x y) (is-member? a x)
      (gt? x y) (is-member? b x)
      :equal true))
  (insert [{a :left y :value b :right :as this} x]
    (cond
      (lt? x y) (->UnbalancedSet (insert a x) y b)
      (gt? x y) (->UnbalancedSet a y (insert b x))
      :equal this))
  SmartSet
  (smart-insert [this x]
    (try
      (smart-insert-helper this x)
      (catch IllegalStateException ise this)))
  (smart-member? [this x]
    (smart-member-helper this x nil)))

(defn- smart-insert-helper [{a :left y :value b :right :as tree} x]
  (cond
    (is-empty? tree) (insert tree x)
    (lt? x y) (->UnbalancedSet (smart-insert-helper a x) y b)
    (gt? x y) (->UnbalancedSet a y (smart-insert-helper b x))
    :equal (throw (IllegalArgumentException. (str x " is already in the set")))))

(extend-type nil
  Set
  (is-empty? [_] true)
  (is-member? [_ _] false)
  (insert [_ x]
    (->UnbalancedSet nil x nil))
  SmartSet
  (smart-member? [_ _] false)
  (smart-insert [_ x]
    (->UnbalancedSet nil x nil)))

(defn unbalanced-set
  ([value] (unbalanced-set nil value nil))
  ([left value right]
    (->UnbalancedSet left value right)))
