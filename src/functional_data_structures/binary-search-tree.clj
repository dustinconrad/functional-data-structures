(ns functional-data-structures.bst)

(defrecord Tree [left value right])

(defprotocol Set
  (is-empty? [this])
  (member? [this value])
  (insert [this value]))

(defn unbalanced-set
  ([val]
    (reify Set
      (is-empty? [_] (nil? val))
      (member? [this x]
        (if (is-empty? this)
          false
          (cond
            (< x (:value val)) (member? (:left val) x)
            (> x (:value val)) (member? (:right val) x)
            :equal true)))
      (insert [this x]
        (if (is-empty? this)
          (unbalanced-set (Tree. (unbalanced-set nil) x (unbalanced-set nil)))
          (cond
            (< x (:value val)) (unbalanced-set (Tree. (insert (:left val) x) (:value val) (:right val)))
            (> x (:value val)) (unbalanced-set (Tree. (:left val) (:value val) (insert (:right val) x)))
            :equal this)))))
  )
