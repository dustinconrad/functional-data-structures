(ns functional-data-structures.rb-tree
  (:require [clojure.core.match :only [match] :as core]
            [functional-data-structures.set :refer :all]
            [functional-data-structures.compare :refer :all]))

(declare lbalance)
(declare rbalance)

(defrecord RedBlackSet [color left value right]
  Set
  (is-empty? [this] (= this (->RedBlackSet :B nil nil nil)))
  (is-member? [{a :left y :value b :right :as tree} x]
    (cond
      (is-empty? tree) false
      (lt? x y) (is-member? a x)
      (gt? x y) (is-member? b x)
      :equal true))
  (insert [s x]
    (let [ins (fn ins [{color :color a :left y :value b :right :as s} x]
                (cond
                  (is-empty? s) (->RedBlackSet :R (->RedBlackSet :B nil nil nil) x (->RedBlackSet :B nil nil nil))
                  (lt? x y) (lbalance color (ins a x) y b)
                  (gt? x y) (rbalance color a y (ins b x))
                  :default s))
          {a-prime :left y-prime :value b-prime :right} (ins s x)]
      (->RedBlackSet :B a-prime y-prime b-prime))))

(defn red-black-set
  ([] (->RedBlackSet :B nil nil nil)))

(defn balance [color left value right]
  (core/match [[color left value right]]
    [(:or [:B {:color :R , :left {:color :R , :left a, :value x, :right b},:value y, :right c} z d]
          [:B {:color :R , :left a, :value x, :right {:color :R , :left b, :value y, :right c}} z d]
          [:B a x {:color :R , :left {:color :R , :left b, :value y, :right c} :value z, :right d}]
          [:B a x {:color :R , :left b, :value y, :right {:color :R , :left c, :value z, :right d}}])]
    (->RedBlackSet :R (->RedBlackSet :B a x b) y (->RedBlackSet :B c z d))
    :else (->RedBlackSet color left value right)))

;exercise 3.9
(defn log [n base]
  (/ (Math/log n) (Math/log base)))

(defn from-ord-list [coll]
  (let [fol (fn fol [n coll]
              (let [color (if (< n 1) :R :B)]
                (cond
                  (empty? coll) nil
                  (= (count coll) 1) (->RedBlackSet color (red-black-set) (first coll) (red-black-set))
                  (= (count coll) 2) (->RedBlackSet color (red-black-set) (first coll) (fol (dec n) (rest coll)))
                  :default (let [half (quot (count coll) 2)
                                 left (take half coll)
                                 mid (nth coll half)
                                 right (drop (inc half) coll)]
                             (->RedBlackSet color
                               (fol (dec n) left)
                               mid
                               (fol (dec n) right))))))]
    (fol (log (count coll) 2) coll)))

;exercise 3.10
;a)
(defn lbalance [color left value right]
  (core/match [[color left value right]]
    [(:or [:B {:color :R , :left {:color :R , :left a, :value x, :right b},:value y, :right c} z d]
          [:B {:color :R , :left a, :value x, :right {:color :R , :left b, :value y, :right c}} z d])]
    (->RedBlackSet :R (->RedBlackSet :B a x b) y (->RedBlackSet :B c z d))
    :else (->RedBlackSet color left value right)))

(defn rbalance [color left value right]
  (core/match [[color left value right]]
    [(:or [:B a x {:color :R , :left {:color :R , :left b, :value y, :right c} :value z, :right d}]
          [:B a x {:color :R , :left b, :value y, :right {:color :R , :left c, :value z, :right d}}])]
    (->RedBlackSet :R (->RedBlackSet :B a x b) y (->RedBlackSet :B c z d))
    :else (->RedBlackSet color left value right)))