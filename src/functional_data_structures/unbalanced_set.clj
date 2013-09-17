(ns functional-data-structures.unbalanced-set
  (:require [functional-data-structures.set :refer :all]))

(defn- lt [x y]
  (neg? (compare x y)))

(defn- gt [x y]
  (pos? (compare x y)))

(defrecord UnbalancedSet [left value right]
  Set
  (is-empty? [_] false)
  (is-member? [{a :left y :value b :right :as this} x]
    (cond
      (lt x y) (is-member? a x)
      (gt x y) (is-member? b x)
      :equal true))
  (insert [{a :left y :value b :right :as this} x]
    (cond
      (lt x y) (->UnbalancedSet (insert a x) y b)
      (gt x y) (->UnbalancedSet a y (insert b x))
      :equal this)))

(extend-protocol Set
  nil
  (is-empty? [_] true)
  (is-member? [_ _] false)
  (insert [this x]
    (->UnbalancedSet nil x nil)))

(defn unbalanced-set
  ([value] (unbalanced-set nil value nil))
  ([left value right]
    (->UnbalancedSet left value right)))
