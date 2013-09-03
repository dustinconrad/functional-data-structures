(ns functional-data-structures.rb-tree)

(defn make-rb-tree
  ([value] (make-tree :red nil value nil))
  ([color left value right]
    {:color color :left left :value value :right right}))

(defn member? [{a :left y :value b :right :as tree} x]
  (cond
    (empty? tree) false
    (< x y) (recur a x)
    (> x y) (recur b x)
    :equal true))