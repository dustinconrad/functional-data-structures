(ns functional-data-structures.unbalanced-set)

(defprotocol Set
  (is-empty? [this])
  (is-member? [this value])
  (insert [this value]))

(defrecord BST [left val right]
  Set
  (is-empty? [_] (nil? val))
  (is-member? [this x]
    (cond
      (.is-empty? this) false
      (< x val) (.is-member? left x)
      (> x val) (.is-member? right x)
      :equal true))
  (insert [this x]
    (cond
      (.is-empty? this) (->BST (->BST nil nil nil) x (->BST nil nil nil))
      (< x val) (->BST (.insert left x) val right)
      (> x val) (->BST left val (.insert right x))
      :equal this))
  )

(defn unbalanced-set
  ([] (unbalanced-set nil nil nil))
  ([val] (unbalanced-set (unbalanced-set) val (unbalanced-set)))
  ([left val right]
    (->BST left val right)))
