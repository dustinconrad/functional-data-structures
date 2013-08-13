(ns functional-data-structures.bst)

(defn make-tree
  ([value] (make-tree nil value nil))
  ([left value right]
    {:left left :value value :right right}))

(defn member? [tree x]
  (cond
    (empty? tree) false
    (< x (:value tree)) (recur (:left tree) x)
    (> x (:value tree)) (recur (:right tree) x)
    :equal true))

(defn insert [tree x]
  (cond
    (empty? tree) (make-tree x)
    (< x (:value tree)) (make-tree (insert (:left tree) x) (:value tree) (:right tree))
    (> x (:value tree)) (make-tree (:left tree) (:value tree) (insert (:right tree) x))
    :equal tree
    ))

(defn- smart-member-helper [tree x max]
  (cond
    (empty? tree) (= x max)
    (<= x (:value tree)) (recur (:left tree) x (:value tree))
    :default (recur (:right tree) x max)))

(defn smart-member? [tree x]
  (smart-member-helper tree x nil))

(defn make-finite-map
  ([key value] (make-finite-map nil key value nil))
  ([left key value right]
    {:left left :key key :value value :right right}))

(defn lookup [f-map key]
  (cond
    (empty? f-map) (throw (Exception. "NotFound"))
    (< key (:key f-map)) (recur (:left f-map) key)
    (> key (:key f-map)) (recur (:right f-map) key)
    :equal (:value f-map)))

(defn bind [f-map key value]
  (cond
    (empty? f-map) (make-finite-map key value)
    (< key (:key f-map)) (make-finite-map (bind (:left f-map) key value) (:key f-map) (:value f-map) (:right f-map))
    (> key (:key f-map)) (make-finite-map (:left f-map) (:key f-map) (:value f-map) (bind (:right f-map) key value))
    :equal (make-finite-map (:left f-map) key value (:right f-map))
    ))