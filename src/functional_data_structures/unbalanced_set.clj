(ns functional-data-structures.unbalanced-set
  (:require [functional-data-structures.set :refer :all ]))

;chapter 2.2

(defn- lt? [x y]
  (neg? (compare x y)))

(defn- gt? [x y]
  (pos? (compare x y)))

(defn- lte? [x y]
  (not (gt? x y)))

(defn- eq? [x y]
  (zero? (compare x y)))

(declare smart-insert-helper)

(declare smart-member-helper)

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
      (smart-insert-helper this x nil)
      (catch IllegalArgumentException iae this)))
  (smart-member? [this x]
    (smart-member-helper this x nil)))

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

;exercise 2.2
(defn- smart-member-helper [{a :left y :value b :right :as tree} x max]
  (cond
    (is-empty? tree) (eq? x max)
    (lte? x y) (recur a x y)
    :default (recur b x max)))

;exercise 2.3 and 2.4
(defn- smart-insert-helper [{a :left y :value b :right :as tree} x max]
  (cond
    (is-empty? tree) (if (= x max) (throw (IllegalArgumentException. (str x " is already in the set"))) (insert tree x))
    (lte? x y) (->UnbalancedSet (smart-insert-helper a x y) y b)
    :default (->UnbalancedSet a y (smart-insert-helper b x max))))

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