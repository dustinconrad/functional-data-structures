(ns functional-data-structures.set)

(defprotocol Set
  (is-empty? [s])
  (is-member? [s value])
  (insert [s value]))

(defprotocol Map
  (is-empty-map? [m])
  (bind [m k v])
  (lookup [m k]))