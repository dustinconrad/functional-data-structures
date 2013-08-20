(ns functional-data-structures.bst)

(defn make-tree
  ([value] (make-tree nil value nil))
  ([left value right]
    {:left left :value value :right right}))

(defn member? [{a :left y :value b :right :as tree} x]
  (cond
    (empty? tree) false
    (< x y) (recur a x)
    (> x y) (recur b x)
    :equal true))

(defn insert [{a :left y :value b :right :as s} x]
  (cond
    (empty? s) (make-tree x)
    (< x y) (make-tree (insert a x) y b)
    (> x y) (make-tree a y (insert b x))
    :equal s
    ))

(defn- smart-member-helper [{left :left value :value right :right :as tree} x max]
  (cond
    (empty? tree) (= x max)
    (<= x value) (recur left x value)
    :default (recur right x max)))

(defn smart-member? [tree x]
  (smart-member-helper tree x nil))

(defn make-finite-map
  ([key value] (make-finite-map nil key value nil))
  ([left key value right]
    {:left left :key key :value value :right right}))

(defn lookup [{l :left k :key v :value r :right :as f-map} key]
  (cond
    (empty? f-map) (throw (Exception. "NotFound"))
    (< key k) (recur l key)
    (> key k) (recur r key)
    :equal v))

(defn bind [{l :left k :key v :value r :right :as f-map} key value]
  (cond
    (empty? f-map) (make-finite-map key value)
    (< key k) (make-finite-map (bind l key value) k v r)
    (> key k) (make-finite-map l k v (bind r key value))
    :equal (make-finite-map l key value r)
    ))