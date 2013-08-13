(ns functional-data-structures.unbalanced-set)

(defprotocol Set
  (is-empty? [this])
  (is-member? [this value])
  (insert [this value]))

(defrecord BST [left value right]
  Set
  (is-empty? [_] (nil? value))
  (is-member? [this x]
    (cond
      (.is-empty? this) false
      (< x value) (.is-member? left x)
      (> x value) (.is-member? right x)
      :equal true))
  (insert [this x]
    (cond
      (.is-empty? this) (->BST (->BST nil nil nil) x (->BST nil nil nil))
      (< x value) (->BST (.insert left x) value right)
      (> x value) (->BST left value (.insert right x))
      :equal this))
  )

(defn unbalanced-set
  ([] (unbalanced-set nil nil nil))
  ([value] (unbalanced-set (unbalanced-set) value (unbalanced-set)))
  ([left value right]
    (->BST left value right)))
