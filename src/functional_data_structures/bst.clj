(ns functional-data-structures.bst)

(defn make-tree
  ([value] (make-tree nil value nil))
  ([left value right]
    {:left left :value value :right right}))

(defn member? [tree x]
  (cond
    (empty? tree) false
    (< x (:value tree)) (member? (:left tree) x)
    (> x (:value tree)) (member? (:right tree) x)
    :equal true))

(defn insert [tree x]
  (cond
    (empty? tree) (make-tree nil x nil)
    (< x (:value tree)) (make-tree (insert (:left tree) x) (:value tree) (:right tree))
    (> x (:value tree)) (make-tree (:left tree) (:value tree) (insert (:right tree) x))
    :equal tree
    ))