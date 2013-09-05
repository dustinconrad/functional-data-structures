(ns functional-data-structures.rb-tree
  (:require [clojure.core.match :only [match] :as core])
  )

(defn make-rb-tree
  ([value] (make-rb-tree :R nil value nil))
  ([color left value right]
    {:color color :left left :value value :right right}))

(defn member? [{a :left y :value b :right :as tree} x]
  (cond
    (empty? tree) false
    (< x y) (recur a x)
    (> x y) (recur b x)
    :equal true))

(defn balance [tree]
  (core/match [tree]
    [(:or {:color :B , :left {:color :R , :left {:color :R , :left a, :value x, :right b},:value y, :right c}, :value z, :right d}
          {:color :B , :left {:color :R , :left a, :value x, :right {:color :R , :left b, :value y, :right c}}, :value z, :right d}
          {:color :B , :left a, :value x, :right {:color :R , :left {:color :R , :left b, :value y, :right c} :value z, :right d}}
          {:color :B , :left a, :value x, :right {:color :R , :left b, :value y, :right {:color :R , :left c, :value z, :right d}}})]
    (make-rb-tree :R (make-rb-tree :B a x b) y (make-rb-tree :B c z d))
    :else tree))

(defn insert [s-prime x-prime]
  (let [ins (fn f [{color :color a :left y :value b :right :as s} x]
              (cond
                (empty? s) (make-rb-tree x)
                (< x y) (balance (make-rb-tree color (f a x) y b))
                (> x y) (balance (make-rb-tree color a y (f b x)))
                :default s))
        {a-prime :left y-prime :value b-prime :right} (ins s-prime x-prime)]
    (make-rb-tree :B a-prime y-prime b-prime)))