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

(defn balance [color left value right]
  (core/match [[color left value right]]
    [(:or [:B {:color :R , :left {:color :R , :left a, :value x, :right b},:value y, :right c} z d]
          [:B {:color :R , :left a, :value x, :right {:color :R , :left b, :value y, :right c}} z d]
          [:B a x {:color :R , :left {:color :R , :left b, :value y, :right c} :value z, :right d}]
          [:B a x {:color :R , :left b, :value y, :right {:color :R , :left c, :value z, :right d}}])]
    (make-rb-tree :R (make-rb-tree :B a x b) y (make-rb-tree :B c z d))
    :else (make-rb-tree color left value right)))

(defn insert [s x]
  (let [ins (fn insrt [{color :color a :left y :value b :right :as s} x]
              (cond
                (empty? s) (make-rb-tree x)
                (< x y) (balance color (insrt a x) y b)
                (> x y) (balance color a y (insrt b x))
                :default s))
        {a-prime :left y-prime :value b-prime :right} (ins s x)]
    (make-rb-tree :B a-prime y-prime b-prime)))

(defn from-ord-list [coll]
  (let [fol (fn fol [coll colors]
              (cond
                (empty? coll) nil
                (= (count coll) 1) (balance (first colors) nil (first coll) nil)
                (= (count coll) 2) (balance (first colors) nil (first coll) (fol (rest coll) (rest colors)))
                :default (let [half (quot (count coll) 2)
                               left (take half coll)
                               mid (nth coll half)
                               right (drop (inc half) coll)]
                           (balance (first colors) (fol left (rest colors)) mid (fol right (rest colors))))))
        {a :left y :value b :right :as tree} (fol coll (cons :B (repeat :R)))]
    (when-not (empty? tree)
      (fol coll (cons :B (repeat :R))))))