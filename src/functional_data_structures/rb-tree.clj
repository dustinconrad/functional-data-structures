(ns functional-data-structures.rb-tree)

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

(defn insert [s-prime x-prime]
  (let [ins (fn [{color :color a :left y :value b :right :as s} x]
              (cond
                (empty? s) (make-rb-tree x)
                (< x y) (balance color (ins a x) y b)
                (>x y) (balance color a y (ins b x))
                :default s))
        {a-prime :left y-prime :value b-prime :right} (ins s-prime x-prime)]
    (make-rb-tree :B a-prime y-prime b-prime)))